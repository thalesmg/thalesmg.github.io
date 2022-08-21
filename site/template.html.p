<!doctype html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="x-ua-compatible" content="ie=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>◊(select-from-metas 'title metas) - thalesmg</title>
        <link rel="stylesheet" href="/css/default.css" />
        <link rel="stylesheet" href="/css/code.css" />
        <link rel="stylesheet" href="/css/gist.css" />
        <style>
        /*! tomorrow night bright; https://github.com/MozMorris/tomorrow-pygments */
        code .highlight, .highlight pre, .highlight table { background: #282a36 !important; color: #eaeaea !important; }
        code .highlight .hll { background-color: #424242 !important; }
        code .highlight .c { color: #969896 !important; } /* Comment */
        code .highlight .err { color: #d54e53 !important; } /* Error */
        code .highlight .k { color: #c397d8 !important; } /* Keyword */
        code .highlight .l { color: #e78c45 !important; } /* Literal */
        code .highlight .n, .highlight .h { color: whitesmoke !important; } /* Name */
        code .highlight .o { color: #70c0b1 !important; } /* Operator */
        code .highlight .p { color: grey !important; } /* Punctuation */
        code .highlight .cm { color: gold !important; } /* Comment.Multiline */
        code .highlight .cp { color: #969896 !important; } /* Comment.Preproc */
        code .highlight .c1 { color: #969896 !important; } /* Comment.Single */
        code .highlight .cs { color: #969896 !important; } /* Comment.Special */
        code .highlight .gd { color: #d54e53 !important; } /* Generic.Deleted */
        code .highlight .ge { font-style: italic !important; } /* Generic.Emph */
        code .highlight .gh { color: #eaeaea !important; font-weight: bold !important; } /* Generic.Heading */
        code .highlight .gi { color: #b9ca4a !important; } /* Generic.Inserted */
        code .highlight .gp { color: #969896 !important; font-weight: bold !important; } /* Generic.Prompt */
        code .highlight .gs { font-weight: bold !important; } /* Generic.Strong */
        code .highlight .gu { color: #70c0b1 !important; font-weight: bold !important; } /* Generic.Subheading */
        code .highlight .kc { color: #c397d8 !important; } /* Keyword.Constant */
        code .highlight .kd { color: #c397d8 !important; } /* Keyword.Declaration */
        code .highlight .kn { color: #70c0b1 !important; } /* Keyword.Namespace */
        code .highlight .kp { color: #c397d8 !important; } /* Keyword.Pseudo */
        code .highlight .kr { color: dodgerblue !important; font-weight: bold; } /* Keyword.Reserved */
        code .highlight .kt { color: #e7c547 !important; } /* Keyword.Type */
        code .highlight .ld { color: #b9ca4a !important; } /* Literal.Date */
        code .highlight .m { color: #e78c45 !important; } /* Literal.Number */
        code .highlight .s { color: burlywood !important; } /* Literal.String */
        code .highlight .na { color: #7aa6da !important; } /* Name.Attribute */
        code .highlight .nb { color: #eaeaea !important; } /* Name.Builtin */
        code .highlight .nc { color: #e7c547 !important; } /* Name.Class */
        code .highlight .no { color: #d54e53 !important; } /* Name.Constant */
        code .highlight .nd { color: #70c0b1 !important; } /* Name.Decorator */
        code .highlight .ni { color: #eaeaea !important; } /* Name.Entity */
        code .highlight .ne { color: #d54e53 !important; } /* Name.Exception */
        code .highlight .nf { color: goldenrod !important; } /* Name.Function */
        code .highlight .nl { color: #eaeaea !important; } /* Name.Label */
        code .highlight .nn { color: gold !important; } /* Name.Namespace */
        code .highlight .nx { color: #7aa6da !important; } /* Name.Other */
        code .highlight .py { color: #eaeaea !important; } /* Name.Property */
        code .highlight .nt { color: #70c0b1 !important; } /* Name.Tag */
        code .highlight .nv { color: #d54e53 !important; } /* Name.Variable */
        code .highlight .ow { color: #70c0b1 !important; } /* Operator.Word */
        code .highlight .w { color: #eaeaea !important; } /* Text.Whitespace */
        code .highlight .mf { color: #e78c45 !important; } /* Literal.Number.Float */
        code .highlight .mh { color: #e78c45 !important; } /* Literal.Number.Hex */
        code .highlight .mi { color: #e78c45 !important; } /* Literal.Number.Integer */
        code .highlight .mo { color: #e78c45 !important; } /* Literal.Number.Oct */
        code .highlight .sb { color: #b9ca4a !important; } /* Literal.String.Backtick */
        code .highlight .sc { color: #eaeaea !important; } /* Literal.String.Char */
        code .highlight .sd { color: #969896 !important; } /* Literal.String.Doc */
        code .highlight .s2 { color: #b9ca4a !important; } /* Literal.String.Double */
        code .highlight .se { color: #e78c45 !important; } /* Literal.String.Escape */
        code .highlight .sh { color: #b9ca4a !important; } /* Literal.String.Heredoc */
        code .highlight .si { color: #e78c45 !important; } /* Literal.String.Interpol */
        code .highlight .sx { color: #b9ca4a !important; } /* Literal.String.Other */
        code .highlight .sr { color: #b9ca4a !important; } /* Literal.String.Regex */
        code .highlight .s1 { color: #b9ca4a !important; } /* Literal.String.Single */
        code .highlight .ss { color: #b9ca4a !important; } /* Literal.String.Symbol */
        code .highlight .bp { color: #eaeaea !important; } /* Name.Builtin.Pseudo */
        code .highlight .vc { color: #d54e53 !important; } /* Name.Variable.Class */
        code .highlight .vg { color: #d54e53 !important; } /* Name.Variable.Global */
        code .highlight .vi { color: #d54e53 !important; } /* Name.Variable.Instance */
        code .highlight .il { color: #e78c45 !important; } /* Literal.Number.Integer.Long */

        </style>
    </head>
    <body class="tj">
        <header>
            <div class="logo">
                <a href="/">thalesmg</a>
            </div>
            <nav>
                <a href="/">Home</a>
                <a href="/about.html">About</a>
                <a href="/archive.html">Archive</a>
            </nav>
        </header>

        <main role="main">
            <h1> ◊(select-from-metas 'title metas) </h1>
            <article>
              ◊(require racket/string)
              ◊when/splice[(select-from-metas 'tags metas)]{
                <div>
                ◊(->html (make-tag-list (select-from-metas 'tags metas)))
                </div>
              }
              <section class="header">
                ◊when/splice[◊(select-from-metas 'date metas)]{
                  Posted on ◊(select-from-metas 'date metas)
                }
              </section>
              <section>
                ◊(parameterize ([current-unescaped-tags '(raw)])
                   (->html doc #:splice? #t))
              </section>
            </article>
        </main>

        <footer>
            Site proudly generated by
            <a href="https://docs.racket-lang.org/pollen/">Pollen</a>
        </footer>
    </body>
</html>
