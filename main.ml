(*
 * Copyright © 2023 Sam Henke
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the “Software”), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 *)

open Base


let calibration_value s =
    let rec loop = function
        | [] -> []
        | '0'..'9' as d :: t -> ((Char.to_int d) - (Char.to_int '0')) :: loop t
        | 'z' :: 'e' :: 'r' :: ('o' :: _ as t) -> 0 :: loop t
        | 'o' :: 'n' :: ('e' :: _ as t) -> 1 :: loop t
        | 't' :: 'w' :: ('o' :: _ as t) -> 2 :: loop t
        | 't' :: 'h' :: 'r' :: 'e' :: ('e' :: _ as t) -> 3 :: loop t
        | 'f' :: 'o' :: 'u' :: 'r' :: t -> 4 :: loop t
        | 'f' :: 'i' :: 'v' :: ('e' :: _ as t) -> 5 :: loop t
        | 's' :: 'i' :: 'x' :: t -> 6 :: loop t
        | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: t -> 7 :: loop t
        | 'e' :: 'i' :: 'g' :: 'h' :: ('t' :: _ as t) -> 8 :: loop t
        | 'n' :: 'i' :: 'n' :: ('e' :: _ as t) -> 9 :: loop t
        | _ :: t -> loop t
    in
    let digits = String.to_list s |> loop in
    (List.last_exn digits) + 10*(List.hd_exn digits)

let () =
    In_channel.fold_lines
        (fun acc line -> acc + (calibration_value line))
        0
        In_channel.stdin
    |> Stdlib.print_int
    |> Stdlib.print_newline
