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
        .highlight, .highlight pre, .highlight table { background: #282a36 !important; color: #eaeaea !important; }
        .highlight .hll { background-color: #424242 !important; }
        .highlight .c { color: #969896 !important; } /* Comment */
        .highlight .err { color: #d54e53 !important; } /* Error */
        .highlight .k { color: #c397d8 !important; } /* Keyword */
        .highlight .l { color: #e78c45 !important; } /* Literal */
        .highlight .n, .highlight .h { color: whitesmoke !important; } /* Name */
        .highlight .o { color: #70c0b1 !important; } /* Operator */
        .highlight .p { color: grey !important; } /* Punctuation */
        .highlight .cm { color: gold !important; } /* Comment.Multiline */
        .highlight .cp { color: #969896 !important; } /* Comment.Preproc */
        .highlight .c1 { color: #969896 !important; } /* Comment.Single */
        .highlight .cs { color: #969896 !important; } /* Comment.Special */
        .highlight .gd { color: #d54e53 !important; } /* Generic.Deleted */
        .highlight .ge { font-style: italic !important; } /* Generic.Emph */
        .highlight .gh { color: #eaeaea !important; font-weight: bold !important; } /* Generic.Heading */
        .highlight .gi { color: #b9ca4a !important; } /* Generic.Inserted */
        .highlight .gp { color: #969896 !important; font-weight: bold !important; } /* Generic.Prompt */
        .highlight .gs { font-weight: bold !important; } /* Generic.Strong */
        .highlight .gu { color: #70c0b1 !important; font-weight: bold !important; } /* Generic.Subheading */
        .highlight .kc { color: #c397d8 !important; } /* Keyword.Constant */
        .highlight .kd { color: #c397d8 !important; } /* Keyword.Declaration */
        .highlight .kn { color: #70c0b1 !important; } /* Keyword.Namespace */
        .highlight .kp { color: #c397d8 !important; } /* Keyword.Pseudo */
        .highlight .kr { color: dodgerblue !important; font-weight: bold; } /* Keyword.Reserved */
        .highlight .kt { color: #e7c547 !important; } /* Keyword.Type */
        .highlight .ld { color: #b9ca4a !important; } /* Literal.Date */
        .highlight .m { color: #e78c45 !important; } /* Literal.Number */
        .highlight .s { color: burlywood !important; } /* Literal.String */
        .highlight .na { color: #7aa6da !important; } /* Name.Attribute */
        .highlight .nb { color: #eaeaea !important; } /* Name.Builtin */
        .highlight .nc { color: #e7c547 !important; } /* Name.Class */
        .highlight .no { color: #d54e53 !important; } /* Name.Constant */
        .highlight .nd { color: #70c0b1 !important; } /* Name.Decorator */
        .highlight .ni { color: #eaeaea !important; } /* Name.Entity */
        .highlight .ne { color: #d54e53 !important; } /* Name.Exception */
        .highlight .nf { color: goldenrod !important; } /* Name.Function */
        .highlight .nl { color: #eaeaea !important; } /* Name.Label */
        .highlight .nn { color: gold !important; } /* Name.Namespace */
        .highlight .nx { color: #7aa6da !important; } /* Name.Other */
        .highlight .py { color: #eaeaea !important; } /* Name.Property */
        .highlight .nt { color: #70c0b1 !important; } /* Name.Tag */
        .highlight .nv { color: #d54e53 !important; } /* Name.Variable */
        .highlight .ow { color: #70c0b1 !important; } /* Operator.Word */
        .highlight .w { color: #eaeaea !important; } /* Text.Whitespace */
        .highlight .mf { color: #e78c45 !important; } /* Literal.Number.Float */
        .highlight .mh { color: #e78c45 !important; } /* Literal.Number.Hex */
        .highlight .mi { color: #e78c45 !important; } /* Literal.Number.Integer */
        .highlight .mo { color: #e78c45 !important; } /* Literal.Number.Oct */
        .highlight .sb { color: #b9ca4a !important; } /* Literal.String.Backtick */
        .highlight .sc { color: #eaeaea !important; } /* Literal.String.Char */
        .highlight .sd { color: #969896 !important; } /* Literal.String.Doc */
        .highlight .s2 { color: #b9ca4a !important; } /* Literal.String.Double */
        .highlight .se { color: #e78c45 !important; } /* Literal.String.Escape */
        .highlight .sh { color: #b9ca4a !important; } /* Literal.String.Heredoc */
        .highlight .si { color: #e78c45 !important; } /* Literal.String.Interpol */
        .highlight .sx { color: #b9ca4a !important; } /* Literal.String.Other */
        .highlight .sr { color: #b9ca4a !important; } /* Literal.String.Regex */
        .highlight .s1 { color: #b9ca4a !important; } /* Literal.String.Single */
        .highlight .ss { color: #b9ca4a !important; } /* Literal.String.Symbol */
        .highlight .bp { color: #eaeaea !important; } /* Name.Builtin.Pseudo */
        .highlight .vc { color: #d54e53 !important; } /* Name.Variable.Class */
        .highlight .vg { color: #d54e53 !important; } /* Name.Variable.Global */
        .highlight .vi { color: #d54e53 !important; } /* Name.Variable.Instance */
        .highlight .il { color: #e78c45 !important; } /* Literal.Number.Integer.Long */

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
                ◊(->html doc #:splice? #t)
              </section>
            </article>
        </main>

        <footer>
            Site proudly generated by
            <a href="https://docs.racket-lang.org/pollen/">Pollen</a>
        </footer>
    </body>
</html>
