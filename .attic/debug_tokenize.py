#!/usr/bin/env python

import tokenize
import cStringIO
import re, hashlib, sys

class TokenSpit():

    def parse_file(self, filename):
        self.parse_string(open(filename).read())

    def parse_string(self, instr):
        self.code_chunks = {}
        instr = self._swap_out_code_chunks(instr)
        string_src = cStringIO.StringIO(instr).readline
        self.tsrc = tokenize.generate_tokens(string_src)
        self._parse(self.tsrc.next())

    def _swap_out_code_chunks(self, instr):
        dynamic_chunks = re.findall(r'(\(>.*?<\)|\$\{.*?\}|\%\{.*?\})', instr, re.DOTALL)
        for chunk in dynamic_chunks:
            tag = '%__dynamic__' + hashlib.md5(chunk).hexdigest()
            instr = instr.replace(chunk, tag)
            self.code_chunks[tag] = chunk
        return instr

    def _parse(self, token):
        if token[0] is tokenize.ENDMARKER:
            print token
            return None
        print token, '('+token[1]+')'
        self._parse(self.tsrc.next())

ts = TokenSpit()
ts.parse_file(sys.argv[1])
