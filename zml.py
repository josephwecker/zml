#!/usr/bin/env python
#
# ZML - Zenlike Markup Language
#
# Some elements and philosophies borrowed from SLiP, haml, sexp2xml (which I
# wrote).  I was piecing this together with various other libraries but finally
# decided I just needed to write a custom parser.
#
# Author: Joseph Wecker
#
#------------------------------------------------------------------------------
# 
# The MIT License
# 
# Copyright (c) 2009 Joseph Wecker
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#

from tokenize import * # mostly for token-type tags
import cStringIO
import re, hashlib, sys
from xml.sax.saxutils import escape, quoteattr

# Not required
import psyco
psyco.full()

def xml_attrib_string(text):
    return quoteattr(escape(text))

def die_error(msg, token=None):
    sys.stderr.write('\nERROR: ' + msg)
    if token:
        sys.stderr.write(' on line %d, char %d [got %d]\n' % (token[2][0],
            token[2][1], token[0]))
        sys.stderr.write(token[4]+'\n')
        sys.stderr.write('-'*token[2][1] + '^\n')
    exit()

class ZmlParser():

    def parse_file(self, filename):
        ''' Take a filename with zml in it and return the xml.  Right
        now it simply reads it in and operates on it as a string- the reason
        being it needs to chunk out the dynamic parts before parsing.  The
        downside is, of course, it may be memory intensive for large files.'''

        return self.parse_string(open(filename).read())

    def parse_string(self, instr):
        ''' Take a zml string and return the xml. '''
        self.code_chunks = {}
        self.code_vars = {}
        self.handlers = {}
        self.misc_data = {}

        self.indent_val = None

        instr = self._swap_out_code_chunks(instr)
        string_src = cStringIO.StringIO(instr).readline
        self.tsrc = generate_tokens(string_src)
        self.parse_tree, _is_eof = self._parse_current_indent_level()
        self._reinsert_code_chunks(self.parse_tree)
        self._process_special_tags(self.parse_tree)
        output = self._build_output(self.parse_tree, 0)
        #print output
        return output

    def _swap_out_code_chunks(self, instr):
        ''' Before it starts tokenizing with the python tokenizer, it needs to
        replace all embedded code chunks with a simple string that can be
        swapped back out later.  Otherwise the tokenizer would try to parse the
        embedded chunk.'''
        # Big chunks of tenjin code
        dynamic_chunks = re.findall(r'\(py>.*?<py\)', instr, re.DOTALL)
        for chunk in dynamic_chunks:
            tag = '`__dynamic__' + hashlib.md5(chunk).hexdigest()
            instr = instr.replace(chunk, tag)
            self.code_chunks[tag] = chunk
        # Block-type sgte code
        dynamic_chunks = re.findall(r'$\s*(\$if .*?\$)\s*^', instr, re.MULTILINE)
        dynamic_chunks.extend(re.findall(r'$\s*(\$else.*?\$)\s*^', instr, re.MULTILINE))
        dynamic_chunks.extend(re.findall(r'$\s*(\$end if\$)\s*^', instr, re.MULTILINE))
        for chunk in dynamic_chunks:
            tag = '@__dynamic__' + hashlib.md5(chunk).hexdigest()
            instr = instr.replace(chunk, tag)
            self.code_chunks[tag] = chunk
        # Tenjin and sgte escaped variables
        dynamic_vars = re.findall(r'(\$\{.*?\}|\%\{.*?}|\$[^$\n\s]+:\{.*?\}.*?\$|\$[\w\s]*\$)', instr, re.DOTALL)
        for var in dynamic_vars:
            tag = '__dynamic__' + hashlib.md5(var).hexdigest()
            instr = instr.replace(var, tag)
            self.code_vars[tag] = var
        return instr

    def _parse_current_indent_level(self, curr_dynamic_level=0):
        ''' Main recursive parsing loop.  (Currently has some redundancy that
        I'll probably be able to factor out at some point).  It recursively
        builds out the list of children at the current level in the parse
        tree.'''
        current_level = []
        while True:
            currt = self._next_token(ignore = [COMMENT, NL, NEWLINE])
            # - Gather 'children' if any
            # - Get dedent or endmarker
            if currt[0] == ENDMARKER:
                return current_level, True
            elif currt[0] == DEDENT:
                return current_level, False
            elif currt[0] == OP:
                # Process a new tag
                curr_tag = {}
                curr_tag['__children'] = []
                curr_tag['__type'] = currt[1]
                curr_tag['__dindlvl'] = curr_dynamic_level
                curr_tag['__full_line'] = currt[4]  # TODO: Only do this when needed
                curr_tag['__name'] = self._next_token(allow_only = [NAME])[1]
                currt = self._next_token()
                if currt[1] == '(':
                    curr_tag = self._get_attributes(curr_tag)
                    currt = self._next_token(ignore = [COMMENT], allow_only =
                            [NAME, STRING, NUMBER, NL, NEWLINE, OP])
                # Otherwise, everything up to the newline is part of children -
                # So we loop through until we hit the EOL and add them on.
                while True:
                    if currt[0] == NL or currt[0] == NEWLINE:
                        break
                    elif currt[0] == OP:
                        curr_tag['__children'].append(self._get_inline_tag(currt[1]))
                    else:
                        curr_tag['__children'].append(self._pull_text(currt))
                    currt = self._next_token(ignore = [COMMENT], allow_only =
                            [NAME, STRING, NUMBER, NL, NEWLINE, OP])
                current_level.append(curr_tag)
            elif currt[0] == INDENT:
                if self.indent_val == None:
                    self.indent_val = currt[1]
                curr_tag = current_level.pop()
                if curr_tag['__type'] == '`' or curr_tag['__type'] == '@':  # Dynamic section tag
                    more_children, cont = \
                            self._parse_current_indent_level(curr_dynamic_level+1)
                else:
                    more_children, cont = \
                            self._parse_current_indent_level(curr_dynamic_level)
                curr_tag['__children'].extend(more_children)
                current_level.append(curr_tag)
                if cont == True:
                    current_level, True
            elif currt[0] == STRING or currt[0] == NUMBER or currt[0] == NAME:
                while True:
                    if currt[0] == NL or currt[0] == NEWLINE:
                        break
                    elif currt[0] == OP:
                        current_level.append(self._get_inline_tag(currt[1]))
                    else:
                        current_level.append(self._pull_text(currt))
                    currt = self._next_token(ignore = [COMMENT], allow_only =
                            [NAME, STRING, NUMBER, NL, NEWLINE, OP])

    def _get_inline_tag(self, tag_type):
        ''' Very, very similar to the main parse loop, but looking for
        (possibly nested) inline tags, which are ended with ';' '''
        curr_tag = {}
        curr_tag['__children'] = []
        curr_tag['__type'] = tag_type
        curr_tag['__dindlvl'] = 'INLINE'
        curr_tag['__full_line'] = ''
        curr_tag['__name'] = self._next_token(allow_only = [NAME])[1]
        currt = self._next_token(ignore = [COMMENT, NL, NEWLINE],
                allow_only = [NAME, STRING, NUMBER, OP])
        while True:
            if currt[1] == '(':
                curr_tag = self._get_attributes(curr_tag)
            elif currt[1] == ';':
                break
            elif currt[0] == OP:
                curr_tag['__children'].append(self._get_inline_tag(currt[1]))
            else:
                curr_tag['__children'].append(self._pull_text(currt))
            currt = self._next_token(ignore = [COMMENT, NL, NEWLINE],
                    allow_only = [NAME, STRING, NUMBER, OP])
        return curr_tag

    def _pull_text(self, token):
        ''' Given a token, it gets the text for the parse tree, depending on
        the kind of token.  It also makes sure and swaps code chunks back into
        the parse tree if there are any.'''
        if token[0] == NAME or token[0] == NUMBER:
            if self.code_vars.has_key(token[1]):
                return self.code_vars[token[1]]
            else:
                return token[1]
        else: # String
            text = token[1]
            for k,v in self.code_vars.items():
                text = text.replace(k, v)
            return self._fix_string(text)

    def _get_attributes(self, cdict):
        ''' Specialized parsing function for gathering in all the attributes,
        until the ')' is encountered.'''
        while True:
            (att_key, att_val) = self._get_single_attribute()
            if att_key == None:
                break
            else:
                cdict['+'+att_key] = att_val
        return cdict

    def _get_single_attribute(self):
        ''' Gets just the next attribute to be found- returning the key and the
        value, or (None, None) if there are no more attributes.'''
        currt = self._next_token(ignore = [COMMENT, NL, NEWLINE])
        if currt[1] == ')':
            return None, None  # Done with attributes
        elif currt[1] == ',':
            # TODO: If someone had a very long string of commas, this would
            # kill the stack.
            return self._get_single_attribute() # just the delim., try again
        elif currt[0] == NAME:
            key = currt[1]
            currt = self._next_token(ignore = [COMMENT],
                    allow_only = [OP], op_allow='=')
            val = self._get_attribute_value()
            if val == None:
                die_error('Attribute needs some sort of value or what\'s the' +
                        ' point?', currt)
            return key, val
        else:
            die_error('Expecting an attribute name', currt)

    def _get_attribute_value(self):
        ''' Even more specialized sub-parser to parse the value of the
        attribute, which can be some kind of string, number, or list of
        values.'''
        currt = self._next_token(allow_only = [OP, NUMBER, STRING, NAME],
                op_allow='[,)')
        if currt[0] == NUMBER or currt[0] == STRING or currt[0] == NAME:
            return self._pull_text(currt)
        elif currt[1] == ',':
            return ''
        elif currt[1] == '[':
            return self._get_list_values()
        return None

    def _get_list_values(self):
        ''' Pulls in list of values until the ']' is reached.'''
        result_values = []
        while True:
            currt = self._next_token(ignore = [COMMENT, NL, NEWLINE],
                    allow_only = [OP, NUMBER, STRING, NAME], op_allow='],')
            if currt[0] == NUMBER or currt[0] == STRING or currt[0] == NAME:
                result_values.append(self._pull_text(currt))
            elif currt[1] == ']':
                return result_values

    def _next_token(self, ignore = [COMMENT], allow_only=None,
            op_allow=None):
        ''' Generic way of pulling the next token from the parse string while
        specifying what's allowed and what's ignored.  Made all the code above
        a lot cleaner and helped with error reporting.'''
        while True:
            token = self.tsrc.next()
            if token[0] not in ignore:
                if allow_only:
                    if token[0] not in allow_only:
                        die_error('I\'m not really sure what you\'re trying to do here', token)
                if op_allow and token[0] == OP:
                    if token[1] not in op_allow:
                        if len(op_allow) == 1:
                            die_error('Expecting "%s"' % op_allow)
                        else:
                            die_error('Expecting one of "%s"' % op_allow)
                break
        return token

    def _reinsert_code_chunks(self, tree_level):
        ''' After the parse tree has been basically built, this puts the
        block-level code chunks into the format they should be in.  A lot of
        the secondary functions here mostly apply to tenjin code chunks.'''
        idx = 0
        for item in tree_level:
            if isinstance(item, dict):
                if item['__type'] == '`':
                    tree_level[idx]['__code_string'] = \
                            self._get_tenjin_code_string(item)
                elif item['__type'] == '@':
                    tree_level[idx]['__code_string'] = \
                            self.code_chunks['@'+item['__name']]
                self._reinsert_code_chunks(tree_level[idx]['__children'])
            idx += 1

    def _get_tenjin_code_string(self, item):
        code_level = item['__dindlvl']
        full_line = item['__full_line']
        orig_code = self.code_chunks['`'+item['__name']]
        formatted_code = self._fix_code_indents(orig_code, full_line)
        formatted_code = self._add_indent(formatted_code, code_level)
        return '<?py\n' + formatted_code + '\n?>\n'

    def _fix_code_indents(self, code, reference_line):
        ''' Reference line shows the intended indent level for when it is
        formatted with the first code line coming right after the start tag, as
        in:
        (py> i = 1 <NL>
        do_some_stuff(i)
        <py)

        In this case we need to see if the second line is indented relative to
        the first line, and we don't know this unless we know the reference
        indent.'''

        orig_indent = re.findall(r'^\s*', reference_line)[0]
        code = orig_indent + code
        code = re.sub(r'\(py\> *', '', code)
        code = re.sub(r'[\t ]*\<py\)', '', code)
        output = []
        for line in code.split('\n'):
            output.append(re.sub('^'+orig_indent, '', line))
        return '\n'.join(output)

    def _add_indent(self, code, code_level):
        indent = code_level * self.indent_val
        if len(indent) > 0:
            output = []
            for line in code.split('\n'):
                output.append(indent + line)
            return '\n'.join(output)
        return code

    def _process_special_tags(self, curr_level):
        ''' Processes specialized tags such as '.someclass (a div)', '%someid
        (a div)', and '*superspecial'- which calls external callbacks.'''
        for child in curr_level:
            if isinstance(child, dict):
                if child['__type'] == '.':
                    if child.has_key('+class'):
                        curr_classes = child['+class'] + ' '
                    else:
                        curr_classes = ''
                    child['+class'] = curr_classes + child['__name']
                    child['__type'] = ':'
                    child['__name'] = 'div'
                elif child['__type'] == '%':
                    child['+id'] = child['__name']
                    child['__type'] = ':'
                    child['__name'] = 'div'
                elif child['__type'] == '*':
                    self._process_super_special(child)
                self._process_special_tags(child['__children'])

    def _process_super_special(self, child_dict):
        ''' Tries to import an appropriate handler, and then uses callbacks to
        modify the child, modify anything in the parse_tree, and build any
        structures that might be used for later emission.'''
        module_name = 'special_handle_' + child_dict['__name']
        if not self.handlers.has_key(module_name):
            self.handlers[module_name] = __import__(module_name)
        self.handlers[module_name].change_parse_tree(child_dict, self.parse_tree)
        self.handlers[module_name].get_misc_data(child_dict, self.misc_data)
        self.handlers[module_name].real_child(child_dict)

    def _build_output(self, curr_level, indlvl):
        ''' Takes the current parse tree and recursively emits it as
        xml.'''
        instance_out = ''
        last_was_norm_child = False
        for child in curr_level:
            if isinstance(child, dict):
                last_was_norm_child = False
                name = child.pop('__name')
                kind = child.pop('__type')
                if kind == '`':
                    instance_out += self._build_code_chunk(curr_level, indlvl, child)
                elif kind == '@':
                    instance_out += child['__code_string']
                    instance_out += self._build_output(child['__children'], indlvl + 1)
                else:
                    code_level = child.pop('__dindlvl') # used for code blocks
                    children = child.pop('__children')
                    full_line = child.pop('__full_line') # used for code blocks

                    if len(child) == 0 and len(children) == 0:
                        instance_out += '<'+name+'/>'
                    elif len(child) == 0:  # i.e., no attributes but has children
                        instance_out += '<'+name+'>'
                        instance_out += self._build_output(children, indlvl + 1)
                        instance_out += '</'+name+'>'
                    elif len(children) == 0: # i.e., has attributes, no children
                        instance_out += '<'+name+' '
                        instance_out += self._build_attributes(child) + '/>'
                    else: # i.e., has attributes and children
                        instance_out += '<' + name + ' ' + self._build_attributes(child) + '>'
                        instance_out += self._build_output(children, indlvl+1)
                        instance_out += '</' + name + '>'
            else:
                if last_was_norm_child:
                    instance_out += ' '
                instance_out += child
                last_was_norm_child = True
        return instance_out

    def _build_code_chunk(self, curr_level, indlvl, child):
        out = child['__code_string']
        out += self._build_output(child['__children'], indlvl+1)
        out += '<?py\n' + self._add_indent('# end', child['__dindlvl']) + '\n?>\n'
        return out

    def _children_has_dict(self, children):
        '''Depricated now- was used when figuring out how to emit
        indentations.'''
        for c in children:
            if isinstance(c, dict):
                return True
        return False

    def _build_attributes(self, attr_list):
        out_str = []
        for k,v in attr_list.items():
            k = k[1:]
            if isinstance(v, list):
                out_str.append(k+'='+xml_attrib_string(' '.join(v)))
            else:
                out_str.append(k+'='+xml_attrib_string(v))
        return ' '.join(out_str)

    def _fix_string(self, string):
        delim = string[0]
        if len(string) > 5 and string[1] == delim and string[2] == delim:
            string = string[3:-3]
        else:
            string = string[1:-1]
            string = string.replace('\\'+delim, delim)
        return string




if __name__ == '__main__':
    zml = ZmlParser()
    zml.parse_file('sample.zml')

