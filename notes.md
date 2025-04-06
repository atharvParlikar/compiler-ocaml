## Grammer for the language (v1)

expression -> literal | unary | binary | grouping;
literal    -> NUMBER | STRING | "true" | "false" | "nil";


factor -> unary (("/" | "*") unary)*
