{ import: st80 }
{ import: Normal }

[
    | n1 n2 |
    'Test' putln.
    (4 * 3.4) putln.
    n1 := Normal x: 1.2 y: 3.0 z: 1.0.
    n2 := Normal x: 1.0 y: 2.0 z: 1.0.
    (n1 dot: n2) printString putln.
    'n1' put. n1 printString putln.
    'n2' put. n2 printString putln.
    (n1 - n2) printString putln.
    (n1 + n2) printString putln.
    n1 add: n2.
    n1 printString putln.
    n2 printString putln.
    (3 * n1) printString putln.
    (n1 * 3.0) printString putln.
    (n2 mult: 2; printString) putln.
    (n2 mult: 0.5; printString) putln.
    (n2 / 2.0) printString putln.
    (n2 / 2) printString putln.
    n2 div: 2.0. 
    n2 negated printString putln.
    'n1' put. n1 printString putln.
    'n2' put. n2 printString putln.
     n1 squaredLength putln.
     n1 length putln.
]
