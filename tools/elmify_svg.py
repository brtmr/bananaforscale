#!/usr/bin/env python3

'''elmify_svg.
In order to use existing svg files dynamically in elm, this module transform
an existing svg file into an elm/svg function
'''

import argparse
import os.path
import re
import xml.etree.ElementTree as ET

supported_tags = [
    'text',
    'node',
    'map',
    'svg',
    'foreignObject',
    'circle',
    'ellipse',
    'image',
    'line',
    'path',
    'polygon',
    'polyline',
    'rect',
    'use',
    'animate',
    'animateColor',
    'animateMotion',
    'animateTransform',
    'mpath',
    'set',
    'desc',
    'metadata',
    'title',
    'a',
    'defs',
    'g',
    'marker',
    'mask',
    'pattern',
    'switch',
    'symbol',
    'altGlyph',
    'altGlyphDef',
    'altGlyphItem',
    'glyph',
    'glyphRef',
    'textPath',
    'text_',
    'tref',
    'tspan',
    'font',
    'linearGradient',
    'radialGradient',
    'stop',
    'feBlend',
    'feColorMatrix',
    'feComponentTransfer',
    'feComposite',
    'feConvolveMatrix',
    'feDiffuseLighting',
    'feDisplacementMap',
    'feFlood',
    'feFuncA',
    'feFuncB',
    'feFuncG',
    'feFuncR',
    'feGaussianBlur',
    'feImage',
    'feMerge',
    'feMergeNode',
    'feMorphology',
    'feOffset',
    'feSpecularLighting',
    'feTile',
    'feTurbulence',
    'feDistantLight',
    'fePointLight',
    'feSpotLight',
    'clipPath',
    'colorProfile',
    'cursor',
    'filter',
    'style',
    'view'
]

supported_attrs = [
   'accentHeight',
   'accelerate',
   'accumulate',
   'additive',
   'alphabetic',
   'allowReorder',
   'amplitude',
   'arabicForm',
   'ascent',
   'attributeName',
   'attributeType',
   'autoReverse',
   'azimuth',
   'baseFrequency',
   'baseProfile',
   'bbox',
   'begin',
   'bias',
   'by',
   'calcMode',
   'capHeight',
   'class',
   'clipPathUnits',
   'contentScriptType',
   'contentStyleType',
   'cx',
   'cy',
   'd',
   'decelerate',
   'descent',
   'diffuseConstant',
   'divisor',
   'dur',
   'dx',
   'dy',
   'edgeMode',
   'elevation',
   'end',
   'exponent',
   'externalResourcesRequired',
   'filterRes',
   'filterUnits',
   'format',
   'from',
   'fx',
   'fy',
   'g1',
   'g2',
   'glyphName',
   'glyphRef',
   'gradientTransform',
   'gradientUnits',
   'hanging',
   'height',
   'horizAdvX',
   'horizOriginX',
   'horizOriginY',
   'id',
   'ideographic',
   'in_',
   'in2',
   'intercept',
   'k',
   'k1',
   'k2',
   'k3',
   'k4',
   'kernelMatrix',
   'kernelUnitLength',
   'keyPoints',
   'keySplines',
   'keyTimes',
   'lang',
   'lengthAdjust',
   'limitingConeAngle',
   'local',
   'markerHeight',
   'markerUnits',
   'markerWidth',
   'maskContentUnits',
   'maskUnits',
   'mathematical',
   'max',
   'media',
   'method',
   'min',
   'mode',
   'name',
   'numOctaves',
   'offset',
   'operator',
   'order',
   'orient',
   'orientation',
   'origin',
   'overlinePosition',
   'overlineThickness',
   'panose1',
   'path',
   'pathLength',
   'patternContentUnits',
   'patternTransform',
   'patternUnits',
   'pointOrder',
   'points',
   'pointsAtX',
   'pointsAtY',
   'pointsAtZ',
   'preserveAlpha',
   'preserveAspectRatio',
   'primitiveUnits',
   'r',
   'radius',
   'refX',
   'refY',
   'renderingIntent',
   'repeatCount',
   'repeatDur',
   'requiredExtensions',
   'requiredFeatures',
   'restart',
   'result',
   'rotate',
   'rx',
   'ry',
   'scale',
   'seed',
   'slope',
   'spacing',
   'specularConstant',
   'specularExponent',
   'speed',
   'spreadMethod',
   'startOffset',
   'stdDeviation',
   'stemh',
   'stemv',
   'stitchTiles',
   'strikethroughPosition',
   'strikethroughThickness',
   'string',
   'style',
   'surfaceScale',
   'systemLanguage',
   'tableValues',
   'target',
   'targetX',
   'targetY',
   'textLength',
   'title',
   'to',
   'transform',
   'type_',
   'u1',
   'u2',
   'underlinePosition',
   'underlineThickness',
   'unicode',
   'unicodeRange',
   'unitsPerEm',
   'vAlphabetic',
   'vHanging',
   'vIdeographic',
   'vMathematical',
   'values',
   'version',
   'vertAdvY',
   'vertOriginX',
   'vertOriginY',
   'viewBox',
   'viewTarget',
   'width',
   'widths',
   'x',
   'xHeight',
   'x1',
   'x2',
   'xChannelSelector',
   'xlinkActuate',
   'xlinkArcrole',
   'xlinkHref',
   'xlinkRole',
   'xlinkShow',
   'xlinkTitle',
   'xlinkType',
   'xmlBase',
   'xmlLang',
   'xmlSpace',
   'y',
   'y1',
   'y2',
   'yChannelSelector',
   'z',
   'zoomAndPan',
   'alignmentBaseline',
   'baselineShift',
   'clipPath',
   'clipRule',
   'clip',
   'colorInterpolationFilters',
   'colorInterpolation',
   'colorProfile',
   'colorRendering',
   'color',
   'cursor',
   'direction',
   'display',
   'dominantBaseline',
   'enableBackground',
   'fillOpacity',
   'fillRule',
   'fill',
   'filter',
   'floodColor',
   'floodOpacity',
   'fontFamily',
   'fontSizeAdjust',
   'fontSize',
   'fontStretch',
   'fontStyle',
   'fontVariant',
   'fontWeight',
   'glyphOrientationHorizontal',
   'glyphOrientationVertical',
   'imageRendering',
   'kerning',
   'letterSpacing',
   'lightingColor',
   'markerEnd',
   'markerMid',
   'markerStart',
   'mask',
   'opacity',
   'overflow',
   'pointerEvents',
   'shapeRendering',
   'stopColor',
   'stopOpacity',
   'strokeDasharray',
   'strokeDashoffset',
   'strokeLinecap',
   'strokeLinejoin',
   'strokeMiterlimit',
   'strokeOpacity',
   'strokeWidth',
   'stroke',
   'textAnchor',
   'textDecoration',
   'textRendering',
   'unicodeBidi',
   'visibility',
   'wordSpacing',
   'writingMode'
]


def main():

    parser = argparse.ArgumentParser()
    parser.add_argument('-i', '--infile', dest='infile', required=True)
    # parser.add_argument('-o', '--outfile', dest='outfile', required=True)

    args = parser.parse_args()
    tree = ET.parse(args.infile)

    print('import Svg')
    print('import Svg.Attributes')
    print('res = ' + elmify(tree.getroot()))


namespace_pattern = re.compile('{(?P<ns>.*)}(?P<tag>[A-Za-z]+)')

def split_namespace_and_tag(fulltag):
    m = re.match(namespace_pattern, fulltag)
    if not m:
        raise ValueError(f'"{fulltag}" does not have a namespace')
    return m.group('ns'), m.group('tag')

def elmify(element, indent=1):
    '''recursively parses an element, and turns it into the appropriate svg
    function'''

    base_indent = 4 * ' '

    fulltag = element.tag
    namespace, tagname = split_namespace_and_tag(fulltag)

    if tagname not in supported_tags:
        return None

    function_name = f'Svg.{tagname}'
    line_start = '\n' + base_indent + ', '

    attrs = dict()
    for key, value in element.attrib.items():
        if re.match(namespace_pattern, key):
            ns, key = split_namespace_and_tag(key)

        if key == 'type':
            key = 'type_'

        if key in supported_attrs:
            attrs[key] = '"' + value + '"'
    attrs = [f'Svg.Attributes.{key} {value}' for key, value in attrs.items()]
    if attrs:
        attrs = '\n' + base_indent + '[' + line_start.join(attrs) + ']'
    else:
        attrs = '[]'

    subelements = []
    for subelement in element:
        if re.match(namespace_pattern, subelement.tag):
            ns, tagname = split_namespace_and_tag(subelement.tag)
        else:
            tagname = subelement.tag

        elmified = elmify(subelement, indent+1)
        if elmified is not None:
            subelements.append(elmified)


    if subelements:
        subelements = '\n' + base_indent + '[' + line_start.join(subelements) + ']'
    else:
        subelements = '[]'


    res = f'''{function_name} {attrs}{subelements}\n'''
    # pad the result with the current level of indentation.
    indent_str = '\n' + indent * base_indent
    res = indent_str.join(res.split('\n'))

    return res




if __name__ == '__main__':
    main()
