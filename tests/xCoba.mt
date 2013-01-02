dim=4;
DefChart[B1,M,{0,1,2,3},{t[],r[],\[Theta][],\[Phi][]},BasisColor->RGBColor[0,0.7,0]];
DefChart[B2,M,{0,1,2,3},{\[Tau][],\[Rho][],\[Theta][],\[Phi][]},BasisColor->RGBColor[1,0,0]];

\[Rho]/:InChart[\[Rho][],B1]=Function[{t,r,\[Theta],\[Phi]},ArcSinh[r/L]];
\[Tau]/:InChart[\[Tau][],B1]=Function[{t,r,\[Theta],\[Phi]},t/L];
r/:InChart[r[],B2]=Function[{\[Tau],\[Rho],\[Theta],\[Phi]},L Sinh[\[Rho]]];
t/:InChart[t[],B2]=Function[{\[Tau],\[Rho],\[Theta],\[Phi]},L \[Tau]];

ComputeBasisValues[B1, B2];

Test[
	Basis[{-a,-B1},{b,B2}]//ComponentArray//ToValues
	,
	{{L^(-1), 0, 0, 0}, {0, 1/(L*Sqrt[1 + r[]^2/L^2]), 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}
	,
	TestID->"xCoba-20130102-Q9K3F5"
]

Test[
	Basis[{-a,-B2},{b,B1}]//ComponentArray//ToValues
	,
	{{L, 0, 0, 0}, {0, L*Cosh[\[Rho][]], 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}
	,
	TestID->"xCoba-20130102-N7D4T5"
]

DefTensor[vector[-a],M];

ImplodedTensorValues[CD, vector, B1];

Test[
	CD[-a]@vector[-b] // Implode // ToBasis[B1] // ComponentArray // First //  First // ToValues
	,
	-(ChristoffelCDPDB1[{0, B1}, {0, -B1}, {0, -B1}]*vector[{0, -B1}]) - 
 	ChristoffelCDPDB1[{1, B1}, {0, -B1}, {0, -B1}]*vector[{1, -B1}] - 
 	ChristoffelCDPDB1[{2, B1}, {0, -B1}, {0, -B1}]*vector[{2, -B1}] - 
 	ChristoffelCDPDB1[{3, B1}, {0, -B1}, {0, -B1}]*vector[{3, -B1}] + PDB1[{0, -B1}][vector[{0, -B1}]]
	,
	TestID->"xCoba-20130102-M4H4Y9"
]

Unset[dim];