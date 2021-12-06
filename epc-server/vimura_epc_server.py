#!/usr/bin/env python
import fitz
# import base64

from epc.server import EPCServer

server = EPCServer(('localhost', 0))

def normalize_edges(page, edges):
    "Transform vimura edges to (normalized) pdf-tools edges."
    size = doc[page].mediabox_size
    return [edges[i]/size[0] if i in [0, 2] else edges[i]/size[1]
            for i in range(0, 4)]

def denormalize_edges(page, edges):
    "Transform (normalized) pdf-tools edges to vimura edges."
    size = doc[page].mediabox_size
    return [edges[i]*size[0] if i in [0, 2] else edges[i]*size[1]
            for i in range(0, 4)]

# @server.register_function
# def test(*args):
#     print(page, args)
#     return page, args

@server.register_function
def open(doc_file):
    global doc
    doc = fitz.open(doc_file)
    return False

# TODO implement
@server.register_function
def close():
    pass

@server.register_function
def save(filepath):
    doc.save("/mnt/4EEDC07F44412A81/git/Masterthesis/test.pdf")
    return "/mnt/4EEDC07F44412A81/git/Masterthesis/test.pdf"

@server.register_function
def number_of_pages():
    return len(doc)

@server.register_function
def pagesize(page):
    size = doc[page].mediabox_size
    return size[0], size[1]

@server.register_function
def renderpage(page, width, *args):
    global p
    print(args)
    p = doc[page]
    # if args:
    #     edges = fitz.Rect(denormalize_edges(page, args[2]))
    #     # edges = p.search_for("and")
    #     try:
    #         # p.add_highlight_annot(edges)
    #         p.draw_rect(edges, 0.5, 0.5, fill_opacity=0.5)
    #     except ValueError:
    #         print("Negelect this error")
    zoom = width/p.mediabox_size[0]
    mat = fitz.Matrix(zoom, zoom)
    # pix = p.get_pixmap(matrix=mat)
    # p.clean_contents()
    # mag = display_width / pix.width
    # svg = page.get_svg_image(matrix=fitz.Matrix(mag, mag))
    # return pix.tobytes("ppm")
    return p.get_svg_image(mat)  # returns size in pt
    # return base64.b64encode(pix.tobytes("png")).decode()
    # return pix.tobytes("png")

# TODO create getannots function producing following response
# (((page . 1) (edges 0.15455 0.190049 0.335979 0.238749) (type . highlight)
#   (id . annot-1-1) (flags . 0) (color . "#fefe00") (contents . "")
#   (modified 25003 54259) (label . "Daniel Nicolai") (subject) (opacity . 1.0)
#   (popup-edges) (popup-is-open) (created)
#   (markup-edges (0.15455 0.190049 0.335979 0.238749)))
#  ((page . 1) (edges 0.199907 0.131846 0.32086 0.180546) (type . highlight)
#   (id . annot-1-0) (flags . 0) (color . "#fefe00") (contents . "")
#   (modified 25003 54232) (label . "Daniel Nicolai") (subject) (opacity . 1.0)
#   (popup-edges) (popup-is-open) (created)
#   (markup-edges (0.199907 0.131846 0.32086 0.180546))))
@server.register_function
def getannots():
    return False

# TODO create pagelinks function producing following response
# (((edges 0.141183 0.14789 0.673246 0.16353) (type . goto-dest) (title . "")
#   (page . 125) (top . 0.144794))
#  ((edges 0.217501 0.165013 0.735103 0.180717) (type . goto-dest) (title . "")
#   (page . 125) (top . 0.402617))
#  ((edges 0.171309 0.182171 0.686421 0.197805) (type . goto-dest) (title . "")
#   (page . 127) (top . 0.394724))
#  ((edges 0.141183 0.213566 0.374606 0.229207) (type . goto-dest) (title . "")
#   (page . 129) (top . 0.144794)))
@server.register_function
def pagelinks():
    return False

@server.register_function
def getselection(page):
    p = doc[page]
    size = doc[page].mediabox_size
    return [[j[i]/size[0] if i in [0, 2] else j[i]/size[1]
             for i in range(0, 4)] for j in p.get_text("blocks")]

@server.register_function
def addannot(page, style, edges):
    p = doc[page]
    edges = fitz.Rect(denormalize_edges(page, edges))
    p.add_highlight_annot(edges)

@server.register_function
def editannot():
    pass

@server.register_function
def delannot():
    pass

server.print_port()
server.serve_forever()
