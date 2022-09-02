from os import path

def setup(app):
    print("caraleooooooooooooooooooooooooooo")
    app.add_html_theme('thalesmg-theme', path.abspath(path.dirname(__file__)))
    return {
        "version": "1.0",
        "parallel_read_safe": True,
    }
