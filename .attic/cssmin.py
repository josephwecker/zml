# Adapted from Borgar: http://stackoverflow.com/questions/222581/python-script-for-minifying-css
# Public Domain as far as I can tell
#

import re

def minify_css(css):
    output = ''
    # remove comments - this will break a lot of hacks :-P
    css = re.sub( r'\s*/\*\s*\*/', "$$HACK1$$", css ) # preserve IE<6 comment hack
    css = re.sub( r'/\*[\s\S]*?\*/', "", css )
    css = css.replace( "$$HACK1$$", '/**/' ) # preserve IE<6 comment hack
    css = re.sub( r'url\((["\'])([^)]*)\1\)', r'url(\2)', css ) # url() doesn't need quotes
    css = re.sub( r'\s+', ' ', css )# spaces may be safely collapsed as generated content will collapse them anyway
    css = re.sub( r'#([0-9a-f])\1([0-9a-f])\2([0-9a-f])\3(\s|;)', r'#\1\2\3\4', css )# shorten collapsable colors: #aabbcc to #abc
    css = re.sub( r':\s*0(\.\d+([cm]m|e[mx]|in|p[ctx]))\s*;', r':\1;', css )# fragment values can loose zeros
    for rule in re.findall( r'([^{]+){([^}]*)}', css ):
        selectors = [re.sub( r'(?<=[\[\(>+=])\s+|\s+(?=[=~^$*|>+\]\)])', r'', selector.strip() ) for selector in rule[0].split( ',' )]# we don't need spaces around operators
        # order is important, but we still want to discard repetitions
        properties = {}
        porder = []
        for prop in re.findall( '(.*?):(.*?)(;|$)', rule[1] ):
            key = prop[0].strip().lower()
            if key not in porder: porder.append( key )
            properties[ key ] = prop[1].strip()
        # output rule if it contains any declarations
        if properties:
            output +=  "%s{%s}" % ( ','.join( selectors ), ''.join(['%s:%s;' % (key, properties[key]) for key in porder])[:-1] )
    return output

if __name__ == '__main__':
    import sys
    print minify_css(open(sys.argv[1]).read())
