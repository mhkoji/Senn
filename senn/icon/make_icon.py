#!/usr/bin/env python3

import os
import subprocess
import textwrap
import fontTools.ttLib
import fontTools.pens.svgPathPen

def write_svg(outpath, fontpath, ch):
    with fontTools.ttLib.TTFont(fontpath) as font:
        glyph_name = font.getBestCmap()[ch]
        glyph_set = font.getGlyphSet()
        glyph = glyph_set[glyph_name]
        pen = fontTools.pens.svgPathPen.SVGPathPen(glyph_set)
        glyph.draw(pen)

        width = glyph.width
        ascender = font['OS/2'].sTypoAscender
        descender = font['OS/2'].sTypoDescender
        height = ascender - descender
        content = textwrap.dedent(f'''
        <svg xmlns="http://www.w3.org/2000/svg"
             viewBox="0 {-ascender} {width} {height}"
             fill="#FFF"
             stroke="#000"
             stroke-width="50">
          <g transform="scale(1, -1)">
            <path d="{pen.getCommands()}"/>
          </g>
        </svg>
        ''')
        with open(outpath, 'w') as f:
            f.write(content)

def main():
    ch = 12379 ## 'せ'
    fontpath = os.path.join(
        os.getcwd(),
        '../third-party/MPLUS_FONTS/Mplus1-Bold.ttf')
    write_svg('./senn.svg', fontpath, ch)

    subprocess.run(['convert',
                    '-size', '40x40',
                    '-gravity', 'center',
                    '-extent', '48x48',
                    '-background', 'none',
                    './senn.svg', './senn.png'])

# pip3 install --user fonttools
# ./make_icon.py
if __name__ == '__main__':
    main()
