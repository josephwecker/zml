#!/usr/bin/env python
#
# ZML - Zenlike Markup Language
#
# Some elements and philosophies borrowed from SLiP, haml, sexp2xml (which I
# wrote).  I was piecing this together with various other libraries but finally
# decided I just needed to write a custom parser.
#
# Author: Joseph Wecker
# Copyright: 2009 Joseph Wecker, All Rights Reserved
#

from tokenize import * # mostly for token-type tags
import cStringIO
import re, hashlib, sys
import pprint
from xml.sax.saxutils import escape, quoteattr

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
        ''' Take a filename with zml in it and return the xml/tenjin.  Right
        now it simply reads it in and operates on it as a string- the reason
        being it needs to chunk out the dynamic parts before parsing.  The
        downside is, of course, it may be memory intensive for large files.'''

        self.parse_string(open(filename).read())

    def parse_string(self, instr):
        ''' Take a zml string and return the xml/tenjin. '''

        self.code_chunks = {}
        self.code_vars = {}
        self.indent_val = None
        instr = self._swap_out_code_chunks(instr)
        string_src = cStringIO.StringIO(instr).readline
        self.tsrc = generate_tokens(string_src)
        self.parse_tree, _is_eof = self._parse_current_indent_level()
        #pp = pprint.PrettyPrinter(indent=2)
        #pp.pprint(self.parse_tree)
        self._reinsert_code_chunks(self.parse_tree)
        output = self._emit_output(self.parse_tree, 0)

    def _reinsert_code_chunks(self, tree_level):
        idx = 0
        for item in tree_level:
            if isinstance(item, dict):
                if item['__type'] == '`':
                    tree_level[idx]['__code_string'] = \
                            self._get_code_string(item)
                self._reinsert_code_chunks(tree_level[idx]['__children'])
            idx += 1

    def _get_code_string(self, item):
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

    def _emit_output(self, curr_level, indlvl):
        ''' Takes the current parse tree and recursively emits it as
        xml/tenjin.'''
        has_out_norm_child = False
        for child in curr_level:
            if isinstance(child, dict):
                name = child.pop('__name')
                kind = child.pop('__type')
                if kind == '`':
                    self._emit_code_chunk(curr_level, indlvl, child)
                # TODO Handle and emit other custom tag types
                else:
                    code_level = child.pop('__dindlvl') # used for code blocks
                    children = child.pop('__children')
                    full_line = child.pop('__full_line') # used for code blocks
                    #if code_level == 'INLINE':
                    #    cind = ''
                    #else:
                    #    cind = indlvl * ' ' * 2
                    cind = ''

                    if len(child) == 0 and len(children) == 0:
                        sys.stdout.write(cind + '<'+name+'/>') #\n')
                    elif len(child) == 0:  # i.e., no attributes but has children
                        sys.stdout.write(cind + '<'+name+'>')
                        if self._children_has_dict(children):
                            #sys.stdout.write('\n')
                            self._emit_output(children, indlvl + 1)
                            sys.stdout.write(cind + '</'+name+'>')#\n')
                        else:
                            self._emit_output(children, indlvl + 1)
                            sys.stdout.write('</'+name+'>')#\n')
                    elif len(children) == 0: # i.e., has attributes, no children
                        sys.stdout.write(cind + '<'+name+' ')
                        self._emit_attributes(child)
                        sys.stdout.write('/>')#\n')
                    else: # i.e., has attributes and children
                        sys.stdout.write(cind + '<'+name+' ')
                        self._emit_attributes(child)
                        sys.stdout.write('>')
                        if self._children_has_dict(children):
                            #sys.stdout.write('\n')
                            self._emit_output(children, indlvl + 1)
                            sys.stdout.write(cind + '</'+name+'>')#\n')
                        else:
                            self._emit_output(children, indlvl + 1)
                            sys.stdout.write('</'+name+'>')#\n')
            else:
                if has_out_norm_child:
                    sys.stdout.write(' ')
                sys.stdout.write(child)
                has_out_norm_child = True

    def _emit_code_chunk(self, curr_level, indlvl, child):
        sys.stdout.write(child['__code_string'])
        self._emit_output(child['__children'], indlvl + 1)
        sys.stdout.write('<?py\n' + self._add_indent('# end',
            child['__dindlvl']) + '\n?>\n')
    
    def _children_has_dict(self, children):
        for c in children:
            if isinstance(c, dict):
                return True
        return False

    def _emit_attributes(self, attr_list):
        out_str = []
        for k,v in attr_list.items():
            k = k[1:]
            if isinstance(v, list):
                out_str.append(k+'='+xml_attrib_string(' '.join(v)))
            else:
                out_str.append(k+'='+xml_attrib_string(v))
        sys.stdout.write(' '.join(out_str))

    def _fix_string(self, string):
        delim = string[0]
        if len(string) > 5 and string[1] == delim and string[2] == delim:
            string = string[3:-3]
        else:
            string = string[1:-1]
            string = string.replace('\\'+delim, delim)
        return string


    def _swap_out_code_chunks(self, instr):
        # Big chunks of code
        dynamic_chunks = re.findall(r'\(py>.*?<py\)', instr, re.DOTALL)
        for chunk in dynamic_chunks:
            tag = '`__dynamic__' + hashlib.md5(chunk).hexdigest()
            instr = instr.replace(chunk, tag)
            self.code_chunks[tag] = chunk
        # Escaped variables
        dynamic_vars = re.findall(r'\$\{.*?\}', instr, re.DOTALL)
        for var in dynamic_vars:
            tag = '__dynamic__' + hashlib.md5(var).hexdigest()
            instr = instr.replace(var, tag)
            self.code_vars[tag] = var
        # Raw variables
        dynamic_vars = re.findall(r'\%\{.*?\}', instr, re.DOTALL)
        for var in dynamic_vars:
            tag = '__dynamic__' + hashlib.md5(var).hexdigest()
            instr = instr.replace(var, tag)
            self.code_vars[tag] = var
        return instr

    def _parse_current_indent_level(self, curr_dynamic_level=0):
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
                if curr_tag['__type'] == '`':  # Dynamic section tag
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
                if vars().has_key('curr_tag'):
                    while True:
                        if currt[0] == NL or currt[0] == NEWLINE:
                            break
                        elif currt[0] == OP:
                            curr_tag['__children'].append(self._get_inline_tag(currt[1]))
                        else:
                            curr_tag['__children'].append(self._pull_text(currt))
                        currt = self._next_token(ignore = [COMMENT], allow_only =
                                [NAME, STRING, NUMBER, NL, NEWLINE, OP])
                        curr_tag['__children'].append(self._pull_text(currt))
                else:
                    current_level.append(self._pull_text(currt))

    def _get_inline_tag(self, tag_type):
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
        if token[0] == NAME or token[0] == NUMBER:
            if self.code_vars.has_key(token[1]):
                return self.code_vars[token[1]]
            else:
                return token[1]
        else:
            text = token[1]
            for k,v in self.code_vars.items():
                text = text.replace(k, v)
            return self._fix_string(text)

    def _get_attributes(self, cdict):
        while True:
            (att_key, att_val) = self._get_single_attribute()
            if att_key == None:
                break
            else:
                cdict['+'+att_key] = att_val
        return cdict

    def _get_single_attribute(self):
        currt = self._next_token(ignore = [COMMENT, NL, NEWLINE])
        if currt[1] == ')':
            return None, None  # Done with attributes
        elif currt[1] == ',':
            # TODO: If someone had a very long string of commas, this would
            # stack-overflow.
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


if __name__ == '__main__':
    zml = ZmlParser()
    zml.parse_file('sample.zml')

