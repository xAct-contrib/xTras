(* Mathematica Test File *)

(* This file contains tests for the core xAct packages, not for xTras. *)


(* ToCanonical 1.1.0 issue reported here: https://groups.google.com/d/msg/xAct/t0vAM34PYYU/yv3xLPvXRywJ *)

DefTensor[S6[a,b,c,d,e,f],M,Symmetric@Range@6];	

Test[
	Block[{$IterationLimit = 500},
		ToCanonical @ Symmetrize @ S6[a,b,c,d,e,f]
	]
	,
	S6[a,b,c,d,e,f]
	,
	TestID->"xAct-20140224-C9X8J2"
]



(* xTensor perfomance issue reported here: https://groups.google.com/d/msg/xAct/ol7hmHXMJxk/wYHXwLefJ7oJ *)

dim = 4;
xAct`xTras`Private`DefBasicDDI[CD];

Test[
	TimeConstrained[
		ToCanonical[
			BasicDDICD4[-a, -b, -c, -d, -e, -f, -g, -h, -i, -j] S2[k, l] CD[i, j]@RiemannCD[e, f, g, h] CD[b, c, d]@S3[a, -k, -l]
		],
		1
	]
	,
	0
	,
	TestID->"xAct-20140224-U4J3L4"
]

dim =. ;

(* ToCanonical on PD reported by Guillaume. *)

DefManifold[E3, 3, {aa, bb, ii, jj}]
DefMetric[1, Metricdelta[-ii,-jj], PD, {",", "\[PartialD]"},FlatMetric -> True]
DefTensor[v[ii], E3]

Test[
	ToCanonical[
		v[aa] v[bb] PD[-ii][v[-bb]] PD[ii][v[-aa]] -
		v[aa] v[-bb] PD[-ii][v[-aa]] PD[ii][v[bb]]
	]
	,
	0
	,
	TestID->"xAct-20140302-Z5O4J5"
]