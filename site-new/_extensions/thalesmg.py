from sphinx.writers.html import HTMLTranslator

class Highlighter(object):
    def __init__(self, highlighter):
        self.highlighter = highlighter

    def highlight_block(self, *args, **kwargs):
        kwargs.setdefault('wrapcode', True)
        return self.highlighter.highlight_block(*args, **kwargs)


class MyHTMLTranslator(HTMLTranslator):
    def __init__(self, nodes, builder, *args, **kwargs):
        HTMLTranslator.__init__(self, nodes, builder, *args, **kwargs)
        self.highlighter = Highlighter(builder.highlighter)


from docutils import nodes
from docutils.parsers.rst.states import Struct

def make_parsed_text_role(class_names=[], node_class=nodes.inline):
    def parsed_text_role(name, rawtext, text, lineno, inliner,
                         options={}, content=[]):
        # Prepare context for nested parsing
        memo = Struct(document=inliner.document,
                      reporter=inliner.reporter,
                      language=inliner.language)

        # Create parent node
        options['classes'] = class_names
        parent = node_class(rawtext, '', **options)

        # Parse role text for markup and add to parent
        processed, messages = inliner.parse(text, lineno, memo, parent)
        parent += processed

        # Return parent node, and any messages from nested parsing
        return [parent], messages

    return parsed_text_role


from docutils import nodes
from docutils.parsers.rst import Directive, directives
from sphinx.util.nodes import set_source_info
from ablog.post import PostNode
from ablog.blog import Blog


class Tag(nodes.General, nodes.Element):
    def __init__(self, tagname, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.tagname = tagname
        self["classes"].extend(["ba", "br3"])


def visit_tag_html(self, node):
    self.body.append(self.starttag(node, "span"))
    self.body.append(node.tagname)
def depart_tag_html(self, node):
    self.body.append("</span>")


class TagLine(nodes.General, nodes.Element):
    pass


def intersperse(iterable, delimiter):
    it = iter(iterable)
    yield next(it)
    for x in it:
        yield delimiter
        yield x


class TagLineDirective(Directive):
    has_content = False
    option_spec = {}

    def run(self):
        node = TagLine()
        node.document = self.state.document
        set_source_info(self, node)
        print("self", self)
        print("dir(self)", dir(self))
        print("state", self.state)
        print("dir(state)", dir(self.state))
        print(node.document)
        print(dir(node.document))
        print(type(node.document))
        print(list(node.document.findall(PostNode)))
        print(list(node.document.findall(PostNode))[0])
        print(dir(list(node.document.findall(PostNode))[0]))
        print(list(node.document.findall(PostNode))[0].attributes['tags'])
        # print(Blog(self.app))
        tags = list(node.document.findall(PostNode))[0].attributes['tags']
        tagos = list(intersperse((Tag(t) for t in tags), nodes.Text(";")))
        return tagos


def jinjaextra(app, docname, source):
    import ablog
    import os
    def get_ablog_path():
        return os.path.dirname(ablog.__file__)
    print(app.builder.templates)
    print(app.builder.templates.environment)
    print(dir(app.builder.templates.environment))
    app.builder.templates.environment.get_ablog_path = get_ablog_path
    # app.builder.templates.environment['get_ablog_path'] = get_ablog_path


def setup(app):
    app.set_translator('html', MyHTMLTranslator)
    app.add_role("nest", make_parsed_text_role(class_names=["nest"]))
    app.add_directive("tagline", TagLineDirective)
    app.add_node(Tag, html=(visit_tag_html, depart_tag_html))
    # app.connect("source-read", jinjaextra)

    return {
        "verison": "1.0",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
