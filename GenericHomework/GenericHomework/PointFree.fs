// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module GenericHomework.PointFree

let func0'1 : int -> int list -> int list =
    fun x -> List.map (fun y -> y * x)

let func0'2 : int -> int list -> int list =
    fun x -> List.map (fun y -> x * y)

let func0'3 : int -> int list -> int list =
    fun x -> List.map (fun y -> ((*) x) y)

let func0'4 : int -> int list -> int list =
    fun x -> List.map ((*) x)

let func0'5 : int -> int list -> int list =
    List.map << (*)

let func : int -> int list -> int list =
    List.map << (*)