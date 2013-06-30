Patty
-----

Patty is a tiny pattern matcher for picolisp.  It is not extensible.

It supports the following patterns:

    literals, list, ?, call, and, or

To match the tail of a list, use a dotted list pattern, eg:

    (patty (list 1 2 3 4)
           ((list X Y . Z) Z))

Evaluates to `(3 4)`.  Patty tries to accomodate picolisp's
interpreted idioms, but since I spend most of my time in compiled,
lexically scoped Lisps it may contain unusual code.  Let me know if
you find any!

