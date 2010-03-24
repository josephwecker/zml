-define(TYPES,[
    {strict,
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"" ++
      " \"http://www.w3.org/TR/html4/strict.dtd\">\n"},
    {transitional,
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"" ++
      " \"http://www.w3.org/TR/html4/loose.dtd\">\n"},
    {frameset,
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"" ++
      " \"http://www.w3.org/TR/html4/frameset.dtd\">\n"},
    {xhtml_strict,
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"" ++
      " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"},
    {xhtml_transitional,
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"" ++
      " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"},
    {xhtml_frameset,
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"" ++
      " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n"},
    {html3,
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"},
    {html2,
      "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"}]).

-define(DEFAULT_TYPE, xhtml_strict).

-define(XMLNS, "http://www.w3.org/1999/xhtml").

-define(JS_LOADER(LibSrc, Inner), [
    "//<![CDATA[\n",
    "var _zmlll=0,_zmljs=document.createElement('script');_zmljs.src='",
    LibSrc,"';",
    "var head=document.getElementsByTagName('head')[0];",
    "head.appendChild(_zmljs);_zmlw();function _zmlw(){",
    "_zmlll?_zmlc():setTimeout('_zmlw()',150)}function _zmlc(){",
    Inner,"};\n", "//]]>\n"]).

-define(ENC_TOP_X(Enc),
  "<?xml version=\"1.0\" encoding=\"" ++ string:to_upper(Enc) ++ "\"?>\n").

-define(ENCODING_DEFAULT, "utf-8").
-define(LANGUAGE_DEFAULT, "en-us").
-define(LANGUAGE_XML_DEFAULT, "en").

-define(STYLESHEET_TYPES,
  ["style", "screen-style", "print-style", "ie-style", "ie-screen-style",
    "ie-print-style"]).

-define(SPECIAL_ATTRIBUTES,
  ["script", "scripts", "type", "encoding", "title"] ++ ?STYLESHEET_TYPES
  ++ [SPATTA++"s" || SPATTA <- ?STYLESHEET_TYPES]).

-define(STYLESHEET_TAGS,
  [{"style",
      {"<script type=\"text/css\">",
        "</script>"}},
    {"screen-style",
      {"<script type=\"text/css\" media=\"screen, projection\">",
        "</script>"}},
    {"print-style",
      {"<script type=\"text/css\" media=\"print\">",
        "</script>"}},
    {"ie-style",
      {"<!--[if IE]><script type=\"text/css\">",
        "</script><![endif]-->"}},
    {"ie-screen-style",
      {"<!--[if IE]><script type=\"text/css\" media=\"screen, projection\">",
        "</script><![endif]-->"}},
    {"ie-print-style",
      {"<!--[if IE]><script type=\"text/css\" media=\"print\">",
        "</script><![endif]-->"}}]).

