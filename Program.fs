module CS220.Program

/// Define a function `prob1` that takes in three (int) numbers as input and
/// returns the sum of the squares of the two large numbers. If there is any
/// error in processing the inputs, e.g., integer overflows, then the function
/// should simply return -1 (do not raise an exception).

let prob1 a b c =
    let square x = x*x
    try
        let aInt = int a
        let bInt = int b
        let cInt = int c
        let sum = 
            if ( aInt > 46341 || aInt < -46341) || ( bInt > 46341 || bInt < -46341) || ( cInt > 46341 || cInt < -46341) then -1
            elif (aInt >= bInt && bInt >= cInt) || (bInt >= aInt && aInt >= cInt) then
               square aInt + square bInt
            elif (aInt >= cInt && cInt >= bInt) || (cInt >= aInt && aInt >= bInt) then 
               square aInt + square cInt
            elif ( bInt >= cInt && cInt >= aInt) || ( cInt >=bInt && bInt >= aInt) then
               square cInt + square bInt 
            else -1
        sum        
    with
    | :? System.OverflowException -> -1
    
/// Define a function `prob2` that takes in a string and returns a new string
/// that ends with a newline character '\n'. The function appends a newline
/// character to the given string only if the string does not already end with a
/// newline character. Note that a string in F# is indeed, an array of
/// characters, and each character in a string can be accessed through an item
/// accessor. For example, str[0] returns the first character of the string str.
/// Also, one can get the length of a string s by calling a function
/// String.length: String.length s returns the length of the string s. Finally,
/// you can append two strings using the + operator.

let prob2 (str: string) =
    if String.length str = 0 then "\n"
    elif str.[str.Length - 1] <> '\n' then str + "\n"
    else str

/// Write a function `prob3` that takes in as input three floating point numbers
/// a, b, and c, and returns a root of the quadratic formula $ax^2 + bx + c =
/// 0$. If there are two roots, then the function should return the bigger root.
/// If there is only one root, then the function should return the root. If
/// there are no real roots, then the function should return nan, which is a
/// special floating point number representing "Not a Number".
let prob3 a b c =
    let d = b*b - 4.0*a*c
    if d > 0.0 then
        let Dis = System.Math.Sqrt(d)
        (-b + Dis ) / (2.0*a)
    elif d = 0.0 then
        -b / (2.0*a)
    else nan

/// Define a function `prob4` that returns the number of days in a month. The
/// function takes in as input an integer representing a month, and outputs the
/// number of days. You can assume that February has 28 days. The function
/// returns -1 for any error cases. For example, if a number big than 12 is
/// given as input, then the function should return -1.
let prob4 month =
    match month with 
    |1 |3 |5 |7 |8 |10 |12 -> 31
    |4 |6 |9 |11 -> 30
    |2 -> 28
    |_ -> -1

[<EntryPoint>]
let main _args =
    0
