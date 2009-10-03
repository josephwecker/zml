# Special handler for '*html' zml tags
# TODO:
#  - Insert docstring and normal html tag
#  - Combine and Embed minimized css into html
#  - Combine and add link to minimized js at bottom of html

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
