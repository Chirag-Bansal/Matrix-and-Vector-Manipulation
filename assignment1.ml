type vector = float list
type matrix = float list list

exception InvalidInput
exception UnequalVectorSize
exception UnequalMatrixShape
exception IncompatibleMatrixShape
exception SingularMatrix

(* This gives the dimention of the vector *)
let rec vdim (x:vector):int = match x with
  [] -> 0
  | x::xs -> 1 + (vdim xs) ;;

 (*These are the test cases*)	
 (*	vdim [1.];;
 *	vdim [1.;2.;3.;4.];;
 *	vdim [];;
 *)

(* This makes a vector full of zeros with length equal to the value given *)
let rec mkzerov (n:int):vector = match n with
  0 -> []
  | x -> 0.0::(mkzerov (x-1));;

(*These are the test cases*)	
 (*	mkzerov 0;;
 *	mkzerov 3;;
 *	mkzerov 1;;
 *)

(* This checks whether this is a zero vector *)
let rec isvzerov (n:vector):bool = match n with
  [0.0] -> true
  | [] -> true
  | x::xs -> if(x = 0.0) then (isvzerov xs) else false;;

(*These are the test cases*)	
 (*	isvzerov [0.];;
 *	isvzerov [0.;0.;0.;0.];;
 *	isvzerov [0.;0.;1.;0.];;
 *)

(*This adds two vectors*)
let rec addv (v1:vector) (v2:vector): vector = match (v1,v2) with
    ([],[]) -> []
    | (x1::xs1, x2::xs2) -> (x1+.x2)::(addv xs1 xs2)
    | (_::_,[]) -> raise UnequalVectorSize
    | ([],_::_) -> raise UnequalVectorSize;;

(*These are the test cases*)	
 (*	addv [0.] [];;
 *	addv [0.] [1.];;
 *	addv [0.;1.;3.] [1.;2.;3.];;
 *)

(*This multiplies a scalar to each element of the vector*)
let rec scalarmultv (c:float) (v:vector): vector = match(c,v) with
  (c,[]) -> []
  | (c,x::xs) -> (c*.x)::(scalarmultv c xs);;

(*These are the test cases*)	
 (*	scalarmultv 0. [];;
 *	scalarmultv 0. [1.];;
 *	scalarmultv 3. [1.;2.;3.];;
 *)

(*This finds the dot product of two vectors*)
let rec dotprodv (v1:vector) (v2:vector): float = match (v1,v2) with
  ([],[]) -> 0.0
  | (x1::xs1,x2::xs2) -> (x1*.x2)+.(dotprodv xs1 xs2)
  | (_::_,[]) -> raise UnequalVectorSize
  | ([],_::_) -> raise UnequalVectorSize;;

(*These are the test cases*)	
 (*	dotprodv [0.] [];;
 *	dotprodv [0.] [1.];;
 *	dotprodv [0.;1.;3.] [1.;2.;3.];;
 *)

(*This finds the cross product of two vectors*)
let crossprodv (v1:vector) (v2:vector): vector = match (v1,v2) with
  (x1::x2::x3::_,y1::y2::y3::_) -> ((x2*.y3)-.(x3*.y2))::((x3*.y1)-.(x1*.y3))::((x1*.y2)-.(x2*.y1))::[]
  |(_::_::_::_, _::_::[])-> raise UnequalVectorSize
  |(_::_::_::_, _::[])-> raise UnequalVectorSize
  |(_::_::_::_, [])-> raise UnequalVectorSize
  |(_::_::[], _)-> raise UnequalVectorSize
  |(_::[], _)-> raise UnequalVectorSize
  |([], _)-> raise UnequalVectorSize;;

(*These are the test cases*)	
 (*	crossprodv [0.] [];;
 *	crossprodv [0.] [1.];;
 *	crossprodv [0.;1.;3.] [1.;2.;3.];;
 *)

(* The functions of vectors are over, these are for matrices *)

(* These are helper functions*)
	let rec vdim2 x = match x with
	  [] -> 0
	  | x::xs -> 1 + (vdim2 xs) ;;

	let rec mkzerov2 n = match n with
	  0 -> []
	  | x -> 0.0::(mkzerov2 (x-1));;

	let rec scalarmultv2 c v = match(c,v) with
	  (c,[]) -> []
	  | (c,x::xs) -> (c*.x)::(scalarmultv2 c xs);;

	let rec isvzerov2 n = match n with
	  [0.0] -> true
	  | [] -> true
	  | x::xs -> if(x = 0.0) then (isvzerov2 xs) else false;;

(*Starting of matrix functions*)

(* Returns a tuple which tells the dimensions of the matrix*)
let mdim (m:matrix): int*int = match m with
  [] -> (0,0)
  |  x::xs ->(1+(vdim2 xs),(vdim x));;

(*These are the test cases*)	
 (*	mdim [[0.];[1.]];;
 *	mdim [[];[]];;
 *	mdim [[0.;1.;9.];[0.;1.;3.];[1.;2.;3.]];;
 *)

(* Returns a matrix with zeros in it*)
let rec mkzerom (m:int) (n:int): matrix = match m with
  1-> [(mkzerov2 n)]
  | x -> (mkzerov2 n)::(mkzerom (x-1) n);;

(*These are the test cases*)	
 (*	mkzerom 1 2;;
 *	mkzerom 0 0;;
 *	mkzerom 5 5;;
 *)

(* Checks if the matrix is a zero matrix*)
let rec iszerom (m:matrix): bool = match m with
  [] -> true
  | x::xs -> if((isvzerov2 x) = true) then (iszerom xs) else false
  | [x] -> (isvzerov2 x);;

(*These are the test cases*)	
 (*	iszerom [[0.];[0.]];;
 *	iszerom [[];[]];;
 *	iszerom [[0.;1.;9.];[0.;1.;3.];[1.;2.;3.]];;
 *	iszerom [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]];;
 *)

(*This adds two matrices together*)
let rec addm (m1:matrix) (m2:matrix): matrix = match (m1,m2) with 
  ([],[]) -> []
  | (_::_, []) -> raise UnequalMatrixShape
  | ([], _) -> raise UnequalMatrixShape
  | ([x1],[x2]) -> (addv x1 x2)::[]
  | (x1::xs1,x2::xs2) -> (addv x1 x2)::(addm xs1 xs2);;

(*These are the test cases*)	
 (*	addm [[0.];[0.]] [[4.];[8.]];;
 *	addm [[0.;1.;9.];[0.;1.;3.];[1.;2.;3.]] [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]];;
 * addm [[];[]] [[0.;1.;9.];[0.;1.;3.];[1.;2.;3.]];;
 *)

(*Multiplies a scalar to all the elements of the matrix*)
let rec scalarmultm (c:float) (m:matrix): matrix = match (c,m) with
  (c,[]) -> []
  | (c,x::xs) -> (scalarmultv2 c x)::(scalarmultm c xs);;

(*These are the test cases*)	
 (*	scalarmultm 6.7 [[0.];[0.]];;
 *	scalarmultm 1.2 [[];[]];;
 *	scalarmultm 0.0 [[0.;1.;9.];[0.;1.;3.];[1.;2.;3.]];;
 *	scalarmultm 8.8 [[1.2;0.;7.8];[9.07;8.;1.];[0.;0.;0.]];;
 *)

(*Creates a vector with given size and 1 at the specified position*)
let rec mkunitv m x = match (m,x) with
| (0,_) -> []
| (m,x) -> if(m=x) then 1.0::(mkunitv (m-1) x) else 0.0::(mkunitv (m-1) x);;

(*This is its helper function*)
let rec mkunitmhelp m x = match (m,x) with
	(_,0) -> []
  | (m,x) -> (mkunitv m x)::(mkunitmhelp m (x-1));;

(*This calls the helper function*)
let mkunitm (m:int):matrix = (mkunitmhelp m m);;

(*These are the test cases*)	
 (*	mkunitm 1 ;;
 *	mkunitm 0 ;;
 *	mkunitm 5 ;;
 *)

(*This is a map function that applies the function f to all the elements in the list*)
let rec map f = function
  | [] -> []
  | a::l -> let r = f a in r :: map f l;;

(*This is the transpose of the matrix*)
let rec transm (l:matrix):matrix = match l with
| []-> []
| []:: xss -> transm xss
| (x::xs) :: xss -> (x :: map List.hd xss) :: transm (xs :: map List.tl xss);;

(*These are the test cases*)	
 (*	transm [[0.];[0.]];;
 *	transm [[];[]];;
 *	transm [[0.;1.;9.];[0.;1.;3.];[1.;2.;3.]];;
 *	transm [[1.2;0.;7.8];[9.07;8.;1.];[0.;0.;0.]];;
 *)

(* These are the hepler functions for the isunitm function that checks whether the function is a unitm or not*)
(* This helper functions checks the given row in the matrix*)
let rec checkrow i n l = match (i,n,l) with
	| (x,n,x1::ys) -> if (i=n && x1=1.0) then true && (checkrow i (n-1) ys) else if (i!=n && x1=0.) then true && (checkrow i (n-1) ys) else false
	| (_,n,[]) -> true;;

(*This checks all the rows in the matrix by calling the above function*)
let rec checkm i n m = match (i,m) with
	(0,_) -> true
	|(_,[]) -> false
	|(x,y::ys) -> (checkrow x n y) && (checkm (x-1) n ys);;

(*This checks if the matrix is square or not*)
let issquare m = match (mdim m) with
	(x,y) -> if (x=y) then true else false;;

(*This is the final function that checks if the matrix is unit or not*)
let rec isunitm (m:matrix):bool = (checkm (vdim2 m) (vdim2 m) m) && (issquare m);;
(*These are the test cases*)	
 (*	isunitm [[0.];[0.]];;
 *	isunitm [[];[]];;
 *	isunitm [[1.;0.;0.];[0.;1.;0.];[0.;0.;1.]];;
 *	isunitm [[1.2;0.;7.8];[9.07;8.;1.];[0.;0.;0.]];;
 *)

(*This removes the element from the row*)
let rec removelem v c i = match(v,i) with
	([],_) -> []
	| (x::xs,i) -> if(i = c) then xs else x::(removelem xs c (i+1));;

(*This removes the cth columns from the matrix*)
let rec removecolumn m c = match m with 
	[] -> []
	| x::xs -> (removelem x c 0)::(removecolumn xs c);;

(*This removes the row of the matrix*)
let removerow v r = (removelem v r 0);;

(*This removes the row and the column of the matrix of the matrix*)
let remrowcol m c r = removecolumn (removerow m r) c;;

(*This sign of the element used in the det and inverse functions*)
let sign c r = if ((c+r) mod 2 = 0) then 1. else -1.;;

(*This gives the first row of the matrix*)
let getfirstrow m = match m with
	[] -> []
	| x::xs -> x;;

(*This gives the ith elements of the row of the matrix*)
let rec getith l i c = match (l,c) with
	([],_) -> 1.
	| (x::xs,c) -> if(i=c) then x else (getith xs i (c+1));;

(*It iterates over the first row and then calculates the determinant and multiplies it with the element*)
let rec dethelp (m: float list list) (i:int):float = match (m,i) with
	([],_) -> 0.
	|([[x]],_) -> x
	|(x::xs,i) -> if(i>vdim2(x)) then 0. else (((dethelp (remrowcol m i 0) 0) *. (sign i 0)) *. (getith (getfirstrow m) i 0)) +.  (dethelp m (i+1)) ;;

(*This return the det of the determinant*)
let rec detm (m:matrix):float = match (mdim m) with
	(x,y) -> if(x = y) then (dethelp m 0) else raise UnequalMatrixShape;;

(*These are the test cases*)	
 (*	detm [[0.];[0.]];;
 *	detm [[];[]];;
 *	detm [[1.;0.;0.];[0.;1.;0.];[0.;0.;1.]];;
 *	detm [[1.2;0.;7.8];[9.07;8.;1.];[0.;0.;0.]];;
 *)

(*This multiplies two lists together*)
let rec multlists l1 l2 = match (l1,l2) with
	([],[]) -> 0.
	| (x::xs,y::ys) -> (x *. y) +. (multlists xs ys)
	| (_::_,[]) -> 0.
	| ([],_::_) -> 0.;;

(*This multiplies a list with a matrix*)
let rec multlistmat l m = match m with
	[] -> []
	| x::xs -> (multlists l x)::(multlistmat l xs);;

let rec multmatmat m1 m2 = match m1 with
	x::xs -> (multlistmat x m2)::(multmatmat xs m2)
	| [] -> [];;

(*This multiplies a matrix with another matrix*)
let multm (m1:matrix) (m2:matrix) :matrix = 
	let (a,b) = mdim m1 and (c,d) = mdim m2 in
	if(b<>c) then raise IncompatibleMatrixShape
	else multmatmat m1 (transm m2);;

(*This return the cofactor of the given element*)
let cofactor_elem m row col = (sign row col) *. (detm (remrowcol m col row) );; 

(*This return the cofactor of the each element in the list*)
let rec cofactor_row m row l i = match l with  
	[] -> []
	| x::xs -> (cofactor_elem m row i):: (cofactor_row m row xs (i+1));;

let rec cofactor m m1 j = match m1 with
	[] -> []
	| x::xs -> (cofactor_row m j x 0)::(cofactor m xs (j+1));;

(*Gives the cofactor matrix*)
let cofac m = cofactor m m 0;;

(*Gives the adjoint matrix*)
let adj m = transm (cofac m);;

(*This return the inverse of the matrix given*)
let invm (m:matrix):matrix = match m with
	[[x]] -> [[1. /. x]]
	| [] -> raise IncompatibleMatrixShape
	| x::xs ->
	let(a,b) = mdim m  and d = detm m in 
	if(a <>b) then raise IncompatibleMatrixShape
	else if(d = 0.) then raise SingularMatrix
	else scalarmultm (1. /. (detm m)) (adj m);;

(*These are the test cases*)	
 (*	invm [[6.]];;
 *	invm [[1.2;0.;7.8];[1.2;0.;7.8];[1.2;0.;7.8]];;
 *	invm [[1.;0.;0.];[0.;1.;0.];[0.;0.;1.]];;
 *	invm [[1.2;0.;7.8];[9.07;8.;1.];[1.;1.;0.]];;
 *)