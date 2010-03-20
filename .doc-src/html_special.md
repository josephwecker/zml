# *html() special tag handler
## Description
This special tag originated as something to take a lot of the redundancy out of
creating html documents so that you could focus on the actual elements that are
unique.  It ended up evolving a number of static compilation abilities as well
that go very far in optimizing documents for speed.

Many of the options are activated by setting particular attributes for the tag
within the zml source.  Other options are available as compilation options, as
explained below.  Among other things, this special handler does the following:

* Lets you skip :head and :body
* Lets you set a 'type' which automatically takes care of DOCTYPE etc.
* Allows easy insertion of javascript libraries
* Automatically finds twin javascript code for code specific to this document
* Can minimize and preprocess javascript, employs tricks for parallel loading, etc.
* Sets namespace and encoding for xhtml
* Allows easy selection of encoding
* Builds appropriate metatags
* Pulls in and pre-compiles ZSS stylesheets
* Removes zss rules that will never be triggered by the current document
* Inlines css as appropriate for speed
* etc.

Right now there is a lot of work being done to integrate a lot of this with
zerl where it can be automated better without resorting to compilation options
etc.

### :head and :body
There is no need to explicitely set a head or a body for the html. The
following are all equivalent:

(Explicitly set head and body)
<code lang="zml">
*html
  :head
  :body
    :a(href:#) Hi there
</code>

(Ignore head)
<code lang="zml">
*html
  :body
    :a(href:#) Hi there
</code>

(ignore both head and body - recommended unless you need something)
<code lang="zml">
*html
  :a(href:#) Hi there
</code>

(Using a head but implied body)
<code lang="zml">
*html
  :head
  :a(href:#) Hi there
</code>

### Doctype handling
Adds a doc-type to the document.  For example:

<code lang="zml">
*html(type: transitional)
  || ...
</code>

This would create html with the following as the first line:

<code lang="html">
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
</code>

Valid values for the 'type' attribute are:

* strict
* transitional
* frameset
* xhtml_strict **(default)**
* xhtml_transitional
* xhtml_frameset
* html3
* html2

They type of document effects very little at this point- some basic things
change based on whether it's an xhtml vs. html, but that's about it.


