# <html> Special Handler
#
# The specialized *html tag in zml can be used to specify css and javascript
# files that will then be optimized automatically.  It also automatically
# puts in the doctype.
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

import os.path
import hashlib
import cssmin, jsmin

jsfile = ''

def get_misc_data(child, misc_data):
    if child.has_key('+scripts'):
        misc_data['jsfile'] = jsfile

def change_parse_tree(child, full_parse_tree):
    # Doctype
    full_parse_tree.insert(0, '<!DOCTYPE HTML>\n')

    # Javascript loaded as a single file at the bottom of the html
    if child.has_key('+scripts'):
        js_fname = combine_scripts(child['+scripts'])
        if js_fname:
            full_parse_tree.append( \
                    {'__name': 'script',
                     '__type': ':',
                     '__children': [''],
                     '__dindlvl': None,
                     '__full_line': None,
                     '+type': 'text/JavaScript',
                     '+src': js_fname})

    # CSS combined and put into header
    if child.has_key('+styles'):
        full_css = ''
        for css in child['+styles']:
            full_css += cssmin.minify_css(open(css).read())
        append_to('head', full_parse_tree, \
                {'__name': 'style',
                 '__type': ':',
                 '__children': [full_css],
                 '__dindlvl': None,
                 '__full_line': None,
                 '+type': 'text/css'})
        
def real_child(child):
    child['__type'] = ':'
    if child.has_key('+styles'):
        child.pop('+styles')
    if child.has_key('+scripts'):
        child.pop('+scripts')
    if child.has_key('+controllers'):
        child.pop('+controllers')



#------------ Helper functions -------------

def append_to(look_for, parse_tree, item, all=False):
    for node in parse_tree:
        if isinstance(node, dict):
            if node['__name'] == look_for:
                node['__children'].append(item)
                if not all:
                    return
            append_to(look_for, node['__children'], item)

def combine_scripts(file_list):
    if not isinstance(file_list, list):
        file_list = [file_list]
    if file_list == []:
        return None
    out_dir = os.path.dirname(file_list[0])
    fnames = [os.path.basename(fname) for fname in file_list]
    fnames.sort()
    new_script_name = out_dir + '/' + hashlib.md5(''.join(fnames)).hexdigest()[:10] + '.js'
    if not os.path.exists(new_script_name):
        full_js = ''
        for js in file_list:
            full_js += jsmin.jsmin(open(js).read()) + '\n\n'
        open(new_script_name, 'w+').writelines(full_js)
    return new_script_name
