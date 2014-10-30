AllContractions::usage = "\!\(\*TagBox[\(AllContractions[ \
\*StyleBox[\"expr\", \"TI\"]] returns a sorted list of all possible full \
contractions of  \*StyleBox[\"expr\", \"TI\"]\(\(\\ over its free \
indices.\)\)\[LineSeparator] AllContractions[ \*StyleBox[\"expr\", \"TI\"],  \
\*StyleBox[\"frees\", \"TI\"]] returns all possible contractions of  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ that have \)\)\*StyleBox[\"frees\", \
\"TI\"]\(\(\\ as free indices.\)\)\[LineSeparator] AllContractions[ \
\*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"frees\", \"TI\"],  \
\*StyleBox[\"sym\", \"TI\"]] returns all possible contractions of  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ with the symmetry \)\)\*StyleBox[\"sym\", \
\"TI\"]\(\(\\ imposed on the free indices \)\)\*StyleBox[\"frees\", \"TI\"] \
.\), DisplayForm]\)"; 
AuxiliaryTensor::usage = "AuxiliaryTensor is an option for AllContractions \
that specifies the name of the auxiliary tensor used for the free indices."; 
BackgroundSolution::usage = "BackgroundSolution is an option for \
ToBackground, PerturbBackground, and ExpandBackground that can be used to \
specify backgrounds values of curvature tensors."; 
BasicDDIDefQ::usage = "\!\(\*TagBox[\(BasicDDIDefQ[ \*StyleBox[\"cd\", \
\"TI\"]] returns True if the basic DDI has been defined for the covariant \
derivative  \*StyleBox[\"cd\", \"TI\"], and False otherwise. \[LineSeparator] \
BasicDDIDefQ[ \*StyleBox[\"cd\", \"TI\"], \*StyleBox[\"d\", \"TI\"]] returns \
True if the basic DDI has been defined for the covariant derivative  \
\*StyleBox[\"cd \", \"TI\"] in  \*StyleBox[\"d\", \"TI\"]\(\(\\ dimensions, \
and False otherwise.\)\)\), DisplayForm]\)"; 
BasicDDI::usage = "BasicDDI is a reserved word in xTras. It is used to label \
the basic dimensional dependent identities associated to different covariant \
derivatives."; 
BasicDDIRelations::usage = "\!\(\*TagBox[\(BasicDDIRelations[] gives \
replacement rules for all defined basic DDIs expressed in metrics. \
\[LineSeparator] BasicDDIRelations[ \*StyleBox[\"cd\", \"TI\"]] gives \
replacement rules for the basic DDI of the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"]\(\(\\ expressed in metrics.\)\)\[LineSeparator] \
BasicDDIRelations[ \*StyleBox[\"cd\", \"TI\"], \*StyleBox[\"d\", \"TI\"]] \
gives replacement rules for the basic DDI of the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"]\(\(\\ expressed in metrics in \
\)\)\*StyleBox[\"d\", \"TI\"]\(\(\\ dimensions.\)\)\), DisplayForm]\)"; 
BasicDDITableaux::usage = "\!\(\*TagBox[\(BasicDDITableaux[ \
\*StyleBox[\"cd\", \"TI\"]] gives the standard Young tableaux of the basic \
DDI of the covariant derivative  \*StyleBox[\"cd\", \"TI\"] . \
\[LineSeparator] BasicDDITableaux[ \*StyleBox[\"cd\", \"TI\"], \
\*StyleBox[\"d\", \"TI\"]] gives the standard Young tableaux of the basic DDI \
of the covariant derivative  \*StyleBox[\"cd\", \"TI\"]\(\(\\ in \
\)\)\*StyleBox[\"d\", \"TI\"]\(\(\\ dimensions.\)\)\), DisplayForm]\)"; 
ClearAutomaticRules::usage = "\!\(\*TagBox[\(ClearAutomaticRules[ \
\*StyleBox[\"symbol\", \"TI\"], \*StyleBox[\"rules\", \"TI\"]] tries to \
remove  \*StyleBox[\"rules\", \"TI\"]\(\(\\ from the upvalues and downvalues \
of \)\)\*StyleBox[\"symbol\", \"TI\"] .\), DisplayForm]\)"; 
ClearCurvatureRelations::usage = "\!\(\*TagBox[\(ClearCurvatureRelations[ \
\*StyleBox[\"cd\", \"TI\"]] removes the automatic curvature relations for the \
covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
ClearSymCovDCache::usage = 
  "ClearSymCovDCache[] clears the cache used by SymmetrizeCovDs."; 
Coefficients::usage = "Coefficients is an option for InvarLagrangian that \
specifies the coefficients in the Lagrangian."; 
CollectConstants::usage = "\!\(\*TagBox[\(CollectConstants[ \
\*StyleBox[\"expr\", \"TI\"]] collects all constant symbols in  \
\*StyleBox[\"expr\", \"TI\"] . \[LineSeparator] CollectConstants[ \
\*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"vars\", \"TI\"]] collects the \
constant symbols  \*Cell[BoxData[StyleBox[\"vars\", \"TI\"]]]\(\(\\ in \
\)\)\*StyleBox[\"expr\", \"TI\"] . \[LineSeparator] CollectConstants[ \
\*StyleBox[\"expr\", \"TI\"], ! \*StyleBox[\"vars\", \"TI\"]] collects all \
constant symbols in  \*StyleBox[\"expr\", \"TI\"]\(\(\\ except \
\)\)\*Cell[BoxData[StyleBox[\"vars\", \"TI\"]]] .\), DisplayForm]\)"; 
CollectMethod::usage = "CollectMethod is an option for CollectTensors that \
specifies what function to apply before collecting the tensors."; 
CollectTensors::usage = "\!\(\*TagBox[\(CollectTensors[ \*StyleBox[\"expr\", \
\"TI\"]] collects all tensorial terms in  \*StyleBox[\"expr\", \"TI\"] . \), \
DisplayForm]\)"; 
ComputeBasisValues::usage = "\!\(\*TagBox[\(ComputeBasisValues[ \
\(\*StyleBox[\"chart\", \"TI\"]\)\_\(\*StyleBox[\"1\", \"TR\"]\), \
\(\*StyleBox[\"chart\", \"TI\"]\)\_\(\*StyleBox[\"2\", \"TR\"]\)] computes \
and stores the values of the basis elements relating  \
\*Cell[BoxData[\(\(\*StyleBox[\"chart\", \"TI\"]\)\_\(\*StyleBox[\"1\", \
\"TR\"]\)\)]]\(\(\\ to \)\)\*Cell[BoxData[\(\(\*StyleBox[\"chart\", \
\"TI\"]\)\_\(\*StyleBox[\"2\", \"TR\"]\)\)]]\(\(\\ and vice versa.\)\)\), \
DisplayForm]\)"; 
ConstantExprQ::usage = "\!\(\*TagBox[\(ConstantExprQ[ \*StyleBox[\"expr\", \
\"TI\"]] returns True if  \*StyleBox[\"expr\", \"TI\"]\(\(\\ only contains \
contains constants (i.e. constant symbols and integers, fractions, etc), and \
False otherwise.\)\)\), DisplayForm]\)"; 
ConstantPrefix::usage = "ConstantPrefix is an option for MakeAnsatz that \
determines the prefix of the constant symbols."; 
ConstantSymbolsOf::usage = "\!\(\*TagBox[\(ConstantSymbolsOf[ \
\*StyleBox[\"expr\", \"TI\"]] returns a list of all non-numeric constant \
symbols in expr. \[LineSeparator] ConstantSymbolsOf[ \(\*StyleBox[\"expr\", \
\"TI\"]\)\_1, \(\*StyleBox[\"expr\", \"TI\"]\)\_2,...] returns a list of all \
non-numeric constant symbols in all of the  \
\*Cell[BoxData[\(\(\*StyleBox[\"expr\", \"TI\"]\)\_\(\*StyleBox[\"i\", \
\"TI\"]\)\)]] .\), DisplayForm]\)"; 
ConstructDDIs::usage = "\!\(\*TagBox[\(ConstructDDIs[ \*StyleBox[\"expr\", \
\"TI\"]] constructs all scalar dimensional dependent identities that can be \
build out of  \*StyleBox[\"expr\", \"TI\"] . \[LineSeparator] ConstructDDIs[ \
\*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"frees\", \"TI\"]] constructs all \
dimensional dependent identities that can be build out of  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ and that have free indices \
\)\)\*StyleBox[\"frees\", \"TI\"] . \[LineSeparator] ConstructDDIs[ \
\*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"frees\", \"TI\"],  \
\*StyleBox[\"sym\", \"TI\"]] constructs all dimensional dependent identities \
that can be build out of  \*StyleBox[\"expr\", \"TI\"]\(\(\\ and that have \
the symmetry \)\)\*StyleBox[\"sym\", \"TI\"]\(\(\\ imposed on their free \
indices \)\)\*StyleBox[\"frees.\", \"TI\"]\), DisplayForm]\)"; 
CurvatureRelationsBianchi::usage = "\!\(\*TagBox[\(CurvatureRelationsBianchi[ \
\*StyleBox[\"cd\", \"TI\"]] gives the contracted second Bianchi identities \
for the curvature tensors of the covariant derivative  \*StyleBox[\"cd\", \
\"TI\"] .  \[LineSeparator] CurvatureRelationsBianchi[ \*StyleBox[\"cd\", \
\"TI\"], Riemann] gives only the identities for the Riemann tensor.  \
\[LineSeparator] CurvatureRelationsBianchi[ \*StyleBox[\"cd\", \"TI\"], \
Ricci] gives only the identities for the Ricci tensor.\), DisplayForm]\)"; 
CurvatureRelationsQ::usage = "\!\(\*TagBox[\(CurvatureRelationsQ[ \
\*StyleBox[\"cd\", \"TI\"]] returns True if all of the curvature relations \
for the covariant derivative  \*StyleBox[\"cd\", \"TI\"]\(\(\\ have been set \
as automatic rules for the curvature tensors, and False otherwise.\)\)\), \
DisplayForm]\)"; 
DefNiceConstantSymbol::usage = "\!\(\*TagBox[\(DefNiceConstantSymbol[ \
\*StyleBox[\"c\", \"TI\"],  \*StyleBox[\"i\", \"TI\"]] defines the constant \
symbol ci that prints as  c\_i . \[LineSeparator] DefNiceConstantSymbol[ \
\*StyleBox[\"c\", \"TI\"],  \*StyleBox[\"i, j\", \"TI\"]] defines the \
constant symbol cij that prints as  c\_i\%j . \[LineSeparator] \
DefNiceConstantSymbol[ \*StyleBox[\"c\", \"TI\"], { \(\*StyleBox[\"i\", \
\"TI\"]\)\_1\*StyleBox[\", \", \"TI\"] \(\*StyleBox[\"i\", \
\"TI\"]\)\_2\*StyleBox[\",...\", \"TI\"]}] defines the constant symbol ci1i2 \
that prints as  c\_\(i\_1 i\_2\) . \[LineSeparator] DefNiceConstantSymbol[ \
\*StyleBox[\"c\", \"TI\"],  { \(\*StyleBox[\"i\", \"TI\"]\)\_1\*StyleBox[\", \
\", \"TI\"] \(\*StyleBox[\"i\", \"TI\"]\)\_2\*StyleBox[\",...\", \"TI\"]}, { \
\(\*StyleBox[\"j\", \"TI\"]\)\_1\*StyleBox[\", \", \"TI\"] \
\(\*StyleBox[\"j\", \"TI\"]\)\_2\*StyleBox[\",...\", \"TI\"]}] defines the \
constant symbol ci1i2j1j2 that prints as  c\_\(i\_1\\ i\_2\)\%\(j\_1\\ j\_2\) \
.\), DisplayForm]\)"; 
DerivativeOrder::usage = "\!\(\*TagBox[\(DerivativeOrder[ \
\*StyleBox[\"expr\", \"TI\"]] gives the order of derivatives of  \
\*StyleBox[\"expr\", \"TI\"] .  \[LineSeparator] DerivativeOrder[ \
\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]] only counts the \
covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
Description::usage = "Description is an option for MapTimed and \
MapTimedIfPlus that specifies what description to show during mapping."; 
DivFreeQ::usage = "\!\(\*TagBox[\(DivFreeQ[ \*StyleBox[\"expr\", \"TI\"], \
\*StyleBox[\"tensor\", \"TI\"]] returns True if  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ does not contain a divergence of \)\)\*StyleBox[\"tensor\", \
\"TI\"]\(\(\\ after arbitrary commutations of derivatives, and False \
otherwise.\)\)\[LineSeparator] DivFreeQ[ \*StyleBox[\"expr\", \"TI\"], \
\*StyleBox[\"tensor\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]] only check the \
covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
EinsteinCC::usage = "\!\(\*TagBox[\(EinsteinCC is a reserved word in  \
\*StyleBox[\"xTras\", Rule[FontSlant, \"Italic\"]] . It is used to generate \
the name of the cosmological Einstein curvature tensor associated to a \
connection acting on a tangent bundle.\), DisplayForm]\)"; 
EinsteinCCToRicci::usage = "\!\(\*TagBox[\(EinsteinCCToRicci[ \
\*StyleBox[\"expr\", \"TI\"]] converts all cosmological Einstein tensors in  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ to Ricci tensors and scalars. \
\)\)\[LineSeparator] EinsteinCCToRicci[ \*StyleBox[\"expr\", \"TI\"], \
\*StyleBox[\"cd\", \"TI\"]] converts only cosmological Einstein tensors of \
the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
EinsteinSpaceRules::usage = "\!\(\*TagBox[\(EinsteinSpaceRules[ \
\*StyleBox[\"cd\", \"TI\"], \*StyleBox[\"K\", \"TI\"]] produces replacement \
rules for the curvature tensors of  \*StyleBox[\"cd\", \"TI\"]\(\(\\ on an \
Einstein space of curvature \)\)\*StyleBox[\"K\", \"TI\"] .\), \
DisplayForm]\)"; 
EulerDensity::usage = "\!\(\*TagBox[\(EulerDensity[ \*StyleBox[\"cd\", \
\"TI\"]] gives the Euler density associated to the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"] . \[LineSeparator] EulerDensity[ \
\*StyleBox[\"cd\", \"TI\"], \*StyleBox[\"dim\", \"TI\"]] gives the Euler \
density associated to the covariant derivative  \*StyleBox[\"cd\", \
\"TI\"]\(\(\\ in the dimension \)\)\*StyleBox[\"dim\", \"TI\"]\(\(\\ if the \
underlying manifold has a generic dimension.\)\)\), DisplayForm]\)"; 
ExpandBackground::usage = "\!\(\*TagBox[\(ExpandBackground[ \
\*StyleBox[\"expr\", \"TI\"]] returns the first order perbutation expansion \
of  \*StyleBox[\"expr\", \"TI\"]\(\(\\ on an arbitrary \
background.\)\)\[LineSeparator] ExpandBackground[ \*StyleBox[\"expr\", \
\"TI\"], \*StyleBox[\"n\", \"TI\"]] returns the  \*StyleBox[\"n\", \"TI\"] th \
order perbutation expansion of  \*StyleBox[\"expr\", \"TI\"]\(\(\\ on an \
arbitrary background.\)\)\), DisplayForm]\)"; 
ExpandFlat::usage = "\!\(\*TagBox[\(ExpandFlat[ \*StyleBox[\"expr\", \"TI\"]] \
returns the first order perbutation expansion of  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ on an flat background.\)\)\[LineSeparator] ExpandFlat[ \
\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"n\", \"TI\"]] returns the  \
\*StyleBox[\"n\", \"TI\"] th order perbutation expansion of  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ on an flat background.\)\)\), \
DisplayForm]\)"; 
ExpandSymCovDs::usage = "\!\(\*TagBox[\(ExpandSymCovDs[ \*StyleBox[\"expr\", \
\"TI\"]] expands all symmetrized covariant derivatives in terms of single \
covariant derivatives. \[LineSeparator] ExpandSymCovDs[ \*StyleBox[\"expr\", \
\"TI\"],  \*StyleBox[\"cd\", \"TI\"]] only expands symmetrized covariant \
derivatives of the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), \
DisplayForm]\)"; 
ExtraRules::usage = "ExtraRules is an option for ToBackground, \
PerturbBackground, and ExpandBackground that can be used to specify \
backgrounds values of tensors."; 
FlatRules::usage = "\!\(\*TagBox[\(FlatRules[ \*StyleBox[\"cd\", \"TI\"]] \
returns replacement rules for the curvature tensors of  \*StyleBox[\"cd\", \
\"TI\"]\(\(\\ on a symmetric space of zero curvature.\)\)\[LineSeparator] \
FlatRules[ \*StyleBox[\"expr\", \"TI\"]] returns replacement rules for the \
curvature tensors of  \*StyleBox[\"cd\", \"TI\"]\(\(\\ on a symmetric space \
of zero curvature for all curvature tensors found in \)\)\*StyleBox[\"expr\", \
\"TI\"] .\), DisplayForm]\)"; 
FreeMetrics::usage = "FreeMetrics is an option for AllContractions that \
restricts the returned contractions to have a certain number of free \
metrics."; 
FromIndexFree::usage = "\!\(\*TagBox[\(FromIndexFree[ \*StyleBox[\"expr\", \
\"TI\"]] converts pseudo-index free terms in  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ to terms in normal notation.\)\)\), DisplayForm]\)"; 
FullSimplification::usage = "\!\(\*TagBox[\(FullSimplification[][ \
\*StyleBox[\"expr\", \"TI\"]] tries to simplify  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ as much as possible, taking Bianchi identities into account and \
sorting covariant derivatives.\)\)\[LineSeparator] FullSimplification[ \
\*StyleBox[\"g\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] only simplifies \
curvature tensors of the metric  \*StyleBox[\"g\", \"TI\"] .\), \
DisplayForm]\)"; 
GradChristoffelToRiemann::usage = "\!\(\*TagBox[\(GradChristoffelToRiemann[ \
\*StyleBox[\"expr\", \"TI\"]] converts partial derivatives of Christoffel \
symbols to Riemann curvatures. \[LineSeparator] GradChristoffelToRiemann[ \
\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]] converts partial \
derivatives of Christoffel symbols of only the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"]\(\(\\ to Riemann curvatures.\)\)\), \
DisplayForm]\)"; 
ImplodedTensorValues::usage = "\!\(\*TagBox[\(ImplodedTensorValues[ \
\*StyleBox[\"cd\", \"TI\"],  \*StyleBox[\"T\", \"TI\"],  \*StyleBox[\"B\", \
\"TI\"]] computes the values of the covariant derivative  \*StyleBox[\"cd\", \
\"TI\"]\(\(\\ of the tensor \)\)\*StyleBox[\"T\", \"TI\"]\(\(\\ in the basis \
\)\)\*StyleBox[\"B.\", \"TI\"]\), DisplayForm]\)"; 
IncludeDuals::usage = "IncludeDuals is an option for SingleInvariants, \
ProductInvariants, and InvarLagrangian that determines whether to include \
dual invariants or not."; 
IndexConfigurations::usage = "\!\(\*TagBox[\(IndexConfigurations[ \
\*StyleBox[\"expr\", \"TI\"]] gives a list of all independent index \
configurations of  \*StyleBox[\"expr\", \"TI\"] .\), DisplayForm]\)"; 
IndexFree::usage = "\!\(\*TagBox[\(IndexFree[ \*StyleBox[\"expr\", \"TI\"]] \
indicates that  \*StyleBox[\"expr\", \"TI\"]\(\(\\ is in pseudo index-free \
notation.\)\)\), DisplayForm]\)"; 
InvarLagrangian::usage = "\!\(\*TagBox[\(InvarLagrangian[ \*StyleBox[\"g\", \
\"TI\"],  \*StyleBox[\"order\", \"TI\"]] gives the most general Lagrangian up \
to  \*StyleBox[\"order\", \"TI\"]\(\(\\ in derivatives of the metric, \
consisting solely of curvature tensors of the metric \)\)\*StyleBox[\"g\", \
\"TI\"] .\), DisplayForm]\)"; 
InvarWrapper::usage = "\!\(\*TagBox[\(InvarWrapper[ \*StyleBox[\"f\", \
\"TI\"], \*StyleBox[\"g\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] returns \
the result of  \*StyleBox[\"f\", \"TI\"][\*StyleBox[\(g,expr\), \"TI\"]] \
while temporarily configuring settings associated to the metric  \
\*StyleBox[\"g\", \"TI\"]\(\(\\ to \)\)\*StyleBox[\"Invar\", Rule[FontSlant, \
\"Italic\"]]\(\(\\ compatible values.\)\)\[LineSeparator] InvarWrapper[ \
\*StyleBox[\"f\", \"TI\"], \*StyleBox[\"g\", \"TI\"]][ \*StyleBox[\"expr\", \
\"TI\"], \*StyleBox[\"args\", \"TI\"]] returns  \*StyleBox[\"f\", \
\"TI\"][\*StyleBox[\"g,expr\", \"TI\"], \*StyleBox[\"args\", \"TI\"]] while \
temporarily configuring settings associated to the metric  \*StyleBox[\"g\", \
\"TI\"]\(\(\\ to \)\)\*StyleBox[\"Invar\", Rule[FontSlant, \"Italic\"]]\(\(\\ \
compatible values.\)\)\), DisplayForm]\)"; 
KillingVectorOf::usage = "\!\(\*TagBox[\(KillingVectorOf is an option for  \
\*Cell[BoxData[\"DefTensor\"], \"InlineFormula\", Rule[FormatType, \
\"StandardForm\"]]\(\(\\ that determines whether the defined tensor is a \
Killing vector of a metric.\)\)\), DisplayForm]\)"; 
KillingVectorQ::usage = "\!\(\*TagBox[\(KillingVectorQ[ \*StyleBox[\"v\", \
\"TI\"]] returns True if the vector  \*StyleBox[\"v\", \"TI\"]\(\(\\ is \
properly defined as a Killing vector, and False \
otherwise.\)\)\[LineSeparator] KillingVectorQ[ \*StyleBox[\"v\", \"TI\"], \
\*StyleBox[\"g\", \"TI\"]] returns True if the vector  \*StyleBox[\"v\", \
\"TI\"]\(\(\\ is defined as a Killing vector of the metric \
\)\)\*StyleBox[\"g\", \"TI\"], and False otherwise. \), DisplayForm]\)"; 
KretschmannToRiemann::usage = "\!\(\*TagBox[\(KretschmannToRiemann[ \
\*StyleBox[\"expr\", \"TI\"]] converts Kretschmann scalars in  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ to Riemann tensors.\)\)\[LineSeparator] \
KretschmannToRiemann[ \*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"cd\", \
\"TI\"]] only converts curvature tensors of the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
LevelSpecQ::usage = "\!\(\*TagBox[\(LevelSpecQ[ \*StyleBox[\"levelspec\", \
\"TI\"]] yields True if  \*StyleBox[\"levelspec\", \"TI\"]\(\(\\ is a valid \
levelspec, and False otherwise.\)\)\), DisplayForm]\)"; 
MakeAnsatz::usage = "\!\(\*TagBox[\(MakeAnsatz[{ \
\*StyleBox[\(\(\*StyleBox[\"e\", \"TI\"]\)\_1\), \"TI\"], \
\*StyleBox[\(\(\*StyleBox[\"e\", \"TI\"]\)\_2\), \"TI\"], \
\*StyleBox[\"\[Ellipsis]\", \"TI\"]}] returns  C\_1\\ e\_1 + C\_2\\ e\_2 \
+\[Ellipsis] , where the  C\_i 's are newly defined constant symbols.\), \
DisplayForm]\)"; 
MakeContractionAnsatz::usage = "\!\(\*TagBox[\(MakeContractionAnsatz[ \
\*StyleBox[\"expr\", \"TI\"]] makes an Ansatz with all possible contractions \
of  \*StyleBox[\"expr\", \"TI\"] . \[LineSeparator] MakeContractionAnsatz[ \
\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"frees\", \"TI\"]] makes an Ansatz \
with all possible contractions of  \*StyleBox[\"expr \", \"TI\"] that have  \
\*StyleBox[\"frees\", \"TI\"]\(\(\\ as free indices.\)\)\[LineSeparator] \
MakeContractionAnsatz[ \*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"frees\", \
\"TI\"],  \*StyleBox[\"sym\", \"TI\"]] makes an Ansatz with all possible \
contractions of  \*StyleBox[\"expr \", \"TI\"] with the symmetry  \
\*StyleBox[\"sym\", \"TI\"]\(\(\\ imposed on the free indices \
\)\)\*StyleBox[\"frees\", \"TI\"] .\), DisplayForm]\)"; 
MakeTraceless::usage = "\!\(\*TagBox[\(MakeTraceless[ \*StyleBox[\"expr\", \
\"TI\"]] returns the traceless version of  \*StyleBox[\"expr\", \"TI\"] .\), \
DisplayForm]\)"; 
ManifestSymmetry::usage = "ManifestSymmetry is an option for YoungSymmetrize, \
YoungProject,  TableauSymmetric, and RiemannYoungProject that specifies with \
what convention the Young tableaux are symmetrized."; 
MapTensors::usage = "\!\(\*TagBox[\(MapTensors[ \*StyleBox[\"f\", \"TI\"], \
\*StyleBox[\"expr\", \"TI\"]] maps  \*StyleBox[\"f\", \"TI\"]\(\(\\ over all \
tensorial expressions in \)\)\*StyleBox[\"expr\", \"TI\"] .\), \
DisplayForm]\)"; 
MapTimedIfPlus::usage = "\!\(\*TagBox[\(MapTimedIfPlus[ \*StyleBox[\"f\", \
\"TI\"],  \*StyleBox[\"expr\", \"TI\"]] applies  \*StyleBox[\"f\", \
\"TI\"]\(\(\\ to each element on the first level in \)\)\*StyleBox[\"expr\", \
\"TI\"]\(\(\\ while monitoring the progress and the estimated time remaining \
only if \)\)\*StyleBox[\"f\", \"TI\"]\(\(\\ has head \
\)\)\*Cell[BoxData[\"Plus\"], \"InlineFormula\", Rule[FormatType, \
\"StandardForm\"]] . \[LineSeparator] MapTimedIfPlus[ \*StyleBox[\"f\", \
\"TI\"],  \*StyleBox[\"expr, levelspec\", \"TI\"]] applies  \*StyleBox[\"f\", \
\"TI\"]\(\(\\ to parts of \)\)\*StyleBox[\"expr\", \"TI\"]\(\(\\ specified by \
\)\)\*StyleBox[\"levelspec\", \"TI\"]\(\(\\ while monitoring the progress and \
the estimated time remaining only if \)\)\*StyleBox[\"f\", \"TI\"]\(\(\\ has \
head \)\)\*Cell[BoxData[\"Plus\"], \"InlineFormula\", Rule[FormatType, \
\"StandardForm\"]] . \), DisplayForm]\)"; 
MapTimed::usage = "\!\(\*TagBox[\(MapTimed[ \*StyleBox[\"f\", \"TI\"],  \
\*StyleBox[\"expr\", \"TI\"]] applies  \*StyleBox[\"f\", \"TI\"]\(\(\\ to \
each element on the first level in \)\)\*StyleBox[\"expr\", \"TI\"]\(\(\\ \
while monitoring the progress and the estimated time \
remaining.\)\)\[LineSeparator] MapTimed[ \*StyleBox[\"f\", \"TI\"],  \
\*StyleBox[\"expr, levelspec\", \"TI\"]] applies  \*StyleBox[\"f\", \
\"TI\"]\(\(\\ to parts of \)\)\*StyleBox[\"expr\", \"TI\"]\(\(\\ specified by \
\)\)\*StyleBox[\"levelspec\", \"TI\"]\(\(\\ while monitoring the progress and \
the estimated time remaining. \)\)\), DisplayForm]\)"; 
MapTimedTensors::usage = "\!\(\*TagBox[\(MapTimedTensors[ \*StyleBox[\"f\", \
\"TI\"], \*StyleBox[\"expr\", \"TI\"]] maps  \*StyleBox[\"f\", \"TI\"]\(\(\\ \
over all tensorial expressions in \)\)\*StyleBox[\"expr\", \"TI\"]\(\(\\ \
while displaying progress information.\)\)\), DisplayForm]\)"; 
MetricOfKillingVector::usage = "\!\(\*TagBox[\(MetricOfKillingVector[ \
\*StyleBox[\"v\", \"TI\"]] returns the metric of which the vector  \
\*StyleBox[\"v\", \"TI\"]\(\(\\ is a Killing vector, and \
\)\)\*Cell[BoxData[\"None\"], \"InlineFormula\", Rule[FormatType, \
\"StandardForm\"]]\(\(\\ if it is not a Killing vector.\)\)\), \
DisplayForm]\)"; 
OrderParameter::usage = "OrderParameter is an option for InvarLagrangian that \
specifies the parameter that labels the order of derivatives."; 
PerturbBackground::usage = "\!\(\*TagBox[\(PerturbBackground[ \
\*StyleBox[\"expr\", \"TI\"]] returns an (unexpanded) first order \
perturbation of  \*StyleBox[\"expr\", \"TI\"]\(\(\\ on an arbitrary \
background. \)\)\[LineSeparator] PerturbBackground[ \*StyleBox[\"expr\", \
\"TI\"], \*StyleBox[\"n\", \"TI\"]] returns an (unexpanded)  \
\*StyleBox[\"n\", \"TI\"] th order perturbation of  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ on an arbitrary background. \)\)\), DisplayForm]\)"; 
PerturbFlat::usage = "\!\(\*TagBox[\(PerturbFlat[ \*StyleBox[\"expr\", \
\"TI\"]] returns an (unexpanded) first order perturbation of  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ on a flat background. \)\)\[LineSeparator] \
PerturbFlat[ \*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"n\", \"TI\"]] returns \
an (unexpanded)  \*StyleBox[\"n\", \"TI\"] th order perturbation of  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ on a flat background. \)\)\), \
DisplayForm]\)"; 
ProductInvariants::usage = "\!\(\*TagBox[\(ProductInvariants[ \
\*StyleBox[\"g\", \"TI\"], \*StyleBox[\"order\", \"TI\"]] gives all products \
of single invariants of the Riemann tensor at  \*StyleBox[\"order\", \
\"TI\"]\(\(\\ in derivatives of the metric \)\)\*StyleBox[\"g\", \"TI\"] .\), \
DisplayForm]\)"; 
RemoveConstants::usage = "\!\(\*TagBox[\(RemoveConstants[ \
\*StyleBox[\"expr\", \"TI\"]] removes all constants from the tensorial \
expression  \*StyleBox[\"expr\", \"TI\"], leaving just the tensors.\), \
DisplayForm]\)"; 
RemoveTensors::usage = "\!\(\*TagBox[\(RemoveTensors[ \*StyleBox[\"expr\", \
\"TI\"]] removes all tensors from  \*StyleBox[\"expr\", \"TI\"], and leaves \
just the constant symbols.\), DisplayForm]\)"; 
RemoveTensorWrapper::usage = "\!\(\*TagBox[\(RemoveTensorWrapper[ \
\*StyleBox[\"expr\", \"TI\"]] removes TensorWrapper heads from  \
\*StyleBox[\"expr\", \"TI\"] . \[LineSeparator] RemoveTensorWrapper is also a \
boolean option for CollectTensors that specifies whether or not to remove the \
TensorWrapper heads after collecting.\), DisplayForm]\)"; 
RicciToEinsteinCC::usage = "\!\(\*TagBox[\(RicciToEinsteinCC[ \
\*StyleBox[\"K\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] converts all Ricci \
tensors in  \*StyleBox[\"expr\", \"TI\"]\(\(\\ to cosmological Einstein \
tensors with cosmological constant \)\)\*StyleBox[\"K\", \"TI\"] .  \
\[LineSeparator] RicciToEinsteinCC[ \*StyleBox[\"K\", \"TI\"]][ \
\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]] converts only Ricci \
tensors of the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), \
DisplayForm]\)"; 
RicciToRiemann::usage = "\!\(\*TagBox[\(RicciToRiemann[ \*StyleBox[\"expr\", \
\"TI\"]] converts Ricci tensors and scalars in  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ to Riemann tensors.\)\)\[LineSeparator] RicciToRiemann[ \
\*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"cd\", \"TI\"]] only converts \
curvature tensors of the covariant derivative  \*StyleBox[\"cd\", \"TI\"] \
.\), DisplayForm]\)"; 
RicciToSchoutenCC::usage = "\!\(\*TagBox[\(RicciToSchoutenCC[ \
\*StyleBox[\"K\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] converts all Ricci \
tensors of in  \*StyleBox[\"expr\", \"TI\"]\(\(\\ to cosmological Schouten \
tensors with cosmological constant \)\)\*StyleBox[\"K\", \"TI\"] .  \
\[LineSeparator] RicciToSchoutenCC[ \*StyleBox[\"K\", \"TI\"]][ \
\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]] converts only Ricci \
tensors of the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), \
DisplayForm]\)"; 
RicciToSchouten::usage = "\!\(\*TagBox[\(RicciToSchouten[ \
\*StyleBox[\"expr\", \"TI\"]] converts all Ricci tensors in  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ to Schouten tensors. \)\)\[LineSeparator] \
RicciToSchouten[ \*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]] \
converts only Ricci tensors of the covariant derivative  \*StyleBox[\"cd\", \
\"TI\"] .\), DisplayForm]\)"; 
RiemannSimplification::usage = "\!\(\*TagBox[\(RiemannSimplification[][ \
\*StyleBox[\"expr\", \"TI\"]] simplifies curvature tensors in  \
\*StyleBox[\"expr\", \"TI\"] . \[LineSeparator] RiemannSimplification[ \
\*StyleBox[\"g\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] only simplifies \
curvature tensors of the metric  \*StyleBox[\"g\", \"TI\"]\(\(\\ in \
\)\)\*StyleBox[\"expr.\", \"TI\"] \[LineSeparator] RiemannSimplification[ \
\*StyleBox[\"g\", \"TI\"], \*StyleBox[\"l\", \"TI\"]][ \*StyleBox[\"expr\", \
\"TI\"]] only simplifies curvature tensors of the metric  \*StyleBox[\"g\", \
\"TI\"]\(\(\\ up to the \)\)\*StyleBox[\"l\", \"TI\"] th step in  \
\*StyleBox[\"expr.\", \"TI\"]\), DisplayForm]\)"; 
RiemannToSymRiemann::usage = "\!\(\*TagBox[\(RiemannToSymRiemann[ \
\*StyleBox[\"expr\", \"TI\"]] converts all Riemann tensors to symmetrized \
Riemann tensors. \[LineSeparator] RiemannToSymRiemann[ \*StyleBox[\"expr\", \
\"TI\"], \*StyleBox[\"cd\", \"TI\"]] only converts Riemann tensors of the \
covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
RiemannYoungProject::usage = "\!\(\*TagBox[\(RiemannYoungProject[ \
\*StyleBox[\"expr\", \"TI\"]] projects all Riemann, Weyl, and symmetrized \
Riemann tensors and their first derivatives in  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ onto their respective Young tableaux. \)\)\[LineSeparator] \
RiemannYoungProject[ \*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"cd\", \
\"TI\"]] only projects curvature tensors of the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
RiemannYoungRule::usage = "\!\(\*TagBox[\(RiemannYoungRule[ \
\*StyleBox[\"cd\", \"TI\"]] gives the projection rules for the Riemann \
tensor, the Weyl tensor, the symmetrized Riemann tensor, and their first \
derivatives of the covariant derivative  \*StyleBox[\"cd\", \"TI\"]\(\(\\ \
onto their respective Young tableaux.\)\)\), DisplayForm]\)"; 
SchoutenCC::usage = "\!\(\*TagBox[\(SchoutenCC is a reserved word in  \
\*StyleBox[\"xTras\", Rule[FontSlant, \"Italic\"]] . It is used to generate \
the name of the cosmological Schouten curvature tensor associated to a \
connection acting on a tangent bundle.\), DisplayForm]\)"; 
SchoutenCCToRicci::usage = "\!\(\*TagBox[\(SchoutenCCToRicci[ \
\*StyleBox[\"expr\", \"TI\"]] converts all cosmological Schouten tensors in  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ to Ricci tensors and scalars. \
\)\)\[LineSeparator] SchoutenCCToRicci[ \*StyleBox[\"expr\", \"TI\"], \
\*StyleBox[\"cd\", \"TI\"]] converts only cosmological Schouten tensors of \
the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
Schouten::usage = "\!\(\*TagBox[\(Schouten is a reserved word in  \
\*StyleBox[\"xTras\", Rule[FontSlant, \"Italic\"]] . It is used to generate \
the name of the Schouten curvature tensor associated to a connection acting \
on a tangent bundle.\), DisplayForm]\)"; 
SchoutenToRicci::usage = "\!\(\*TagBox[\(SchoutenToRicci[ \
\*StyleBox[\"expr\", \"TI\"]] converts all Schouten tensors in  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ to Ricci tensors and scalars. \
\)\)\[LineSeparator] SchoutenToRicci[ \*StyleBox[\"expr\", \"TI\"], \
\*StyleBox[\"cd\", \"TI\"]] converts only Schouten tensors of the covariant \
derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
SetCurvatureRelations::usage = "\!\(\*TagBox[\(SetCurvatureRelations[ \
\*StyleBox[\"cd\", \"TI\"]] sets the automatic curvature relations for the \
covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
SimplifyMethod::usage = "SimplifyMethod is an option for CollectTensors and \
CollectConstants that specifies how collected prefactors are simplified."; 
SingleInvariants::usage = "\!\(\*TagBox[\(SingleInvariants[ \*StyleBox[\"g\", \
\"TI\"], \*StyleBox[\"order\", \"TI\"]] gives a basis of single invariants of \
the Riemann tensor at  \*StyleBox[\"order\", \"TI\"]\(\(\\ in derivatives of \
the metric \)\)\*StyleBox[\"g\", \"TI\"] .\), DisplayForm]\)"; 
SolveConstants::usage = "\!\(\*TagBox[\(SolveConstants[ \*StyleBox[\"expr\", \
\"TI\"]] attempts to solve the system  \*StyleBox[\"expr\", \"TI\"]\(\(\\ of \
tensorial equations for all constant symbols appearing in \
\)\)\*StyleBox[\"expr\", \"TI\"] . \[LineSeparator] SolveConstants[ \
\*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"vars\", \"TI\"]] attempts to \
solve the system  \*StyleBox[\"expr\", \"TI\"]\(\(\\ of tensorial equations \
for constant symbols \)\)\*StyleBox[\"vars\", \"TI\"] .  \[LineSeparator] \
SolveConstants[ \*StyleBox[\"expr\", \"TI\"], ! \*StyleBox[\"vars\", \"TI\"]] \
attempts to solve the system  \*StyleBox[\"expr\", \"TI\"]\(\(\\ of tensorial \
equations for all constant symbols appearing in \)\)\*StyleBox[\"expr\", \
\"TI\"], except  \*StyleBox[\"vars\", \"TI\"] .\), DisplayForm]\)"; 
SolveTensors::usage = "\!\(\*TagBox[\(SolveTensors[ \*StyleBox[\"expr\", \
\"TI\"]] attempts to solve the system  \*StyleBox[\"expr\", \"TI\"]\(\(\\ of \
tensorial equations for all tensors in \)\)\*StyleBox[\"expr\", \"TI\"] . \
\[LineSeparator] SolveTensors[ \*StyleBox[\"expr\", \"TI\"],  \
\*StyleBox[\"tens\", \"TI\"]] attempts to solve the system  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ of tensorial equations for the tensors \
\)\)\*StyleBox[\"tens\", \"TI\"] .\), DisplayForm]\)"; 
SortCovDsToBox::usage = "\!\(\*TagBox[\(SortCovDsToBox[ \
\*StyleBox[\"tensor\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] commutes \
derivatives in  \*StyleBox[\"expr\", \"TI\"]\(\(\\ such that all possible \
boxes act on the specified tensor. \)\)\[LineSeparator] SortCovDsToBox[ \
\*StyleBox[\"tensor\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]][ \
\*StyleBox[\"expr\", \"TI\"]] only commutes the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
SortCovDsToDiv::usage = "\!\(\*TagBox[\(SortCovDsToDiv[ \
\*StyleBox[\"tensor\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] commutes \
derivatives in  \*StyleBox[\"expr\", \"TI\"]\(\(\\ such that any derivative \
contracted with \)\)\*StyleBox[\"tensor\", \"TI\"]\(\(\\ acts directy on the \
specified tensor. \)\)\[LineSeparator] SortCovDsToDiv[ \*StyleBox[\"tensor\", \
\"TI\"], \*StyleBox[\"cd\", \"TI\"]][ \*StyleBox[\"expr\", \"TI\"]] only \
commutes the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), \
DisplayForm]\)"; 
SortedCovDsQ::usage = "\!\(\*TagBox[\(SortedCovDsQ[ \*StyleBox[\"expr\", \
\"TI\"]] returns True if the  \*StyleBox[\"expr\", \"TI\"]\(\(\\ has all its \
covariant derivatives sorted, and False otherwise. \)\)\[LineSeparator] \
SortedCovDsQ[ \*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"cd\", \"TI\"]] only \
checks the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), \
DisplayForm]\)"; 
SortMethod::usage = "SortMethod is an option for SolveTensors that determines \
which tensors get solved first."; 
SymCovDQ::usage = "\!\(\*TagBox[\(SymCovDQ[ \*StyleBox[\"cd\", \"TI\"]] gives \
True if  \*StyleBox[\"cd\", \"TI\"]\(\(\\ has been defined as a symmetrizable \
covariant derivative, and False otherwise.\)\)\[LineSeparator] SymCovDQ is \
also an option for DefCovD specifying whether the derivative to be defined is \
symmetrizable or not.\), DisplayForm]\)"; 
SymmetricSpaceRules::usage = "\!\(\*TagBox[\(SymmetricSpaceRules[ \
\*StyleBox[\"cd\", \"TI\"], \*StyleBox[\"K\", \"TI\"]] produces replacement \
rules for the curvature tensors of the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"]\(\(\\ on a symmetric space of constant curvature \
\)\)\*StyleBox[\"K\", \"TI\"] .\), DisplayForm]\)"; 
SymmetrizeCovDs::usage = "\!\(\*TagBox[\(SymmetrizeCovDs[ \
\*StyleBox[\"expr\", \"TI\"]] symmetrizes all symmetrizable covariant \
derivatives in  \*StyleBox[\"expr. \", \"TI\"] \[LineSeparator] \
SymmetrizeCovDs[ \*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"cd\", \"TI\"]] \
only symmetrizes the covariant derivative  \*StyleBox[\"cd.\", \"TI\"]\), \
DisplayForm]\)"; 
SymmetrizeMethod::usage = "SymmetrizeMethod is an option for AllContractions \
that specifies how it should symmetrize the free indices."; 
SymRiemann::usage = "\!\(\*TagBox[\(SymRiemann is a reserved word in  \
\*StyleBox[\"xTras\", Rule[FontSlant, \"Italic\"]] . It is used to generate \
the name of the symmetrized Riemann curvature tensor associated to a \
connection acting on a tangent bundle.\), DisplayForm]\)"; 
SymRiemannToRiemann::usage = "\!\(\*TagBox[\(SymRiemannToRiemann[ \
\*StyleBox[\"expr\", \"TI\"]] converts all symmetrized Riemann tensors to \
Riemann tensors. \[LineSeparator] SymRiemannToRiemann[ \*StyleBox[\"expr\", \
\"TI\"], \*StyleBox[\"cd\", \"TI\"]] only converts symmetrized Riemann \
tensors of the covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), \
DisplayForm]\)"; 
TableauDimension::usage = "\!\(\*TagBox[\(TableauDimension[ \
\*StyleBox[\"tab\", \"TI\"]] gives the dimension of the Young diagram \
associated to the Young tableau  \*StyleBox[\"tab\", \"TI\"] .\), \
DisplayForm]\)"; 
TableauSymmetric::usage = "\!\(\*TagBox[\(TableauSymmetric[ \
\*StyleBox[\"tab\", \"TI\"]] gives the symmetry of the tableau  \
\*StyleBox[\"tab\", \"TI\"]\(\(\\ as a Strong Generating Set.\)\)\), \
DisplayForm]\)"; 
TensorWrapper::usage = "\!\(\*TagBox[\(TensorWrapper[ \*StyleBox[\"expr\", \
\"TI\"]] wraps all tensorial terms in  \*StyleBox[\"expr\", \"TI\"]\(\(\\ in \
the inert head \)\)\*StyleBox[\"TensorWrapper\", \"InlineCode\"] .\), \
DisplayForm]\)"; 
TermsOf::usage = "\!\(\*TagBox[\(TermsOf[ \*StyleBox[\"expr\", \"TI\"]] gives \
all different tensorial terms in  \*StyleBox[\"expr\", \"TI\"]\(\(\\ in \
pseudo index-free notation.\)\)\), DisplayForm]\)"; 
TimeString::usage = "\!\(\*TagBox[\(TimeString[ \*StyleBox[\"seconds\", \
\"TI\"]] formats  \*StyleBox[\"seconds\", \"TI\"]\(\(\\ \*\"as a string of \
the form \\\"\"\)\)\*StyleBox[\"10 minutes, 5 seconds\", Rule[FontSlant, \
\"Italic\"]] \*\"\\\".\"\), DisplayForm]\)"; 
ToBackground::usage = "\!\(\*TagBox[\(ToBackground[ \*StyleBox[\"expr\", \
\"TI\"]] sends unperturbed (curvature) tensors to background values.\), \
DisplayForm]\)"; 
ToConstantSymbolEquations::usage = "\!\(\*TagBox[\(ToConstantSymbolEquations[ \
\*StyleBox[\"expr\", \"TI\"]] takes the system  \*StyleBox[\"expr\", \
\"TI\"]\(\(\\ of tensorial equations and turns it into a system of equations \
for the constant symbols appearing \)\)\*StyleBox[\"expr\", \"TI\"] .\), \
DisplayForm]\)"; 
ToFlat::usage = "\!\(\*TagBox[\(ToFlat[ \*StyleBox[\"expr\", \"TI\"]] ensures \
 \*StyleBox[\"expr\", \"TI\"]\(\(\\ is on a flat background.\)\)\), \
DisplayForm]\)"; 
ToIndexFree::usage = "\!\(\*TagBox[\(ToIndexFree[ \*StyleBox[\"expr\", \
\"TI\"]] converts  \*StyleBox[\"expr\", \"TI\"]\(\(\\ to pseudo-index free \
notation.\)\)\), DisplayForm]\)"; 
ToLevelSpec::usage = "\!\(\*TagBox[\(ToLevelSpec[ \*StyleBox[\"levelspec\", \
\"TI\"]] converts a valid  \*StyleBox[\"levelspec\", \"TI\"]\(\(\\ into the \
{m,n} format.\)\)\), DisplayForm]\)"; 
ToRicci::usage = "\!\(\*TagBox[\(ToRicci[ \*StyleBox[\"expr\", \"TI\"]] \
converts all curvature tensors of rank two and contracted Riemann tensors in  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ to Ricci tensors and scalars. \
\)\)\[LineSeparator] ToRicci[ \*StyleBox[\"expr\", \"TI\"],  \
\*StyleBox[\"cd\", \"TI\"]] converts only for curvature tensors of the \
covariant derivative  \*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
ToRiemann::usage = "\!\(\*TagBox[\(ToRiemann[ \*StyleBox[\"expr\", \"TI\"]] \
converts as many curvature tensors to Riemann tensors as possible. \
\[LineSeparator] ToRiemann[ \*StyleBox[\"expr\", \"TI\"],  \*StyleBox[\"cd\", \
\"TI\"]] only converts curvature tensors of the covariant derivative  \
\*StyleBox[\"cd\", \"TI\"] .\), DisplayForm]\)"; 
UncontractedIndices::usage = "UncontractedIndices is an option for \
AllContractions which specifies how many indices should not be contracted."; 
UnitConstant::usage = 
  "UnitConstant is a constant symbol whose value is one."; 
VarL::usage = "\!\(\*TagBox[\(VarL[ \*StyleBox[\"g\", \
\"TI\"][-\*StyleBox[\"a\", \"TI\"],- \*StyleBox[\"b\", \"TI\"]]][ \
\*StyleBox[\"L\", \"TI\"]] returns  \
1\/\@\(\[VerticalSeparator]g\[VerticalSeparator]\) \(\[Delta] ( \
\@\(-g\)\(\(\\ L) \)\)\)\/\(\[Delta]  g\_ab\)\(\(\\ \\ while integrating by \
parts with respect to the covariant derivative of the metric \
\)\)\*StyleBox[\(g\_ab\), \"TI\"] . \[LineSeparator] VarL[ \*StyleBox[\"g\", \
\"TI\"][-\*StyleBox[\"a\", \"TI\"],- \*StyleBox[\"b\", \"TI\"]], \
\*StyleBox[\"cd\", \"TI\"]][ \*StyleBox[\"L\", \"TI\"]] returns  \
1\/\@\(\[VerticalSeparator]g\[VerticalSeparator]\) \(\[Delta] ( \
\@\(-g\)\(\(\\ L) \)\)\)\/\(\[Delta]  g\_ab\)\(\(\\ while integrating by \
parts with respect to the covariant derivative \)\)\*StyleBox[\"cd\", \"TI\"] \
.\), DisplayForm]\)"; 
YoungProject::usage = "\!\(\*TagBox[\(YoungProject[ \*StyleBox[\"expr\", \
\"TI\"], \*StyleBox[\"tab\", \"TI\"]] projects the tensorial expression  \
\*StyleBox[\"expr\", \"TI\"]\(\(\\ onto the Young tableau \
\)\)\*StyleBox[\"tab\", \"TI\"] . \), DisplayForm]\)"; 
YoungSymmetrize::usage = "\!\(\*TagBox[\(YoungSymmetrize[ \
\*StyleBox[\"expr\", \"TI\"], \*StyleBox[\"tab\", \"TI\"]] symmetrizes the \
tensorial expression  \*StyleBox[\"expr\", \"TI\"]\(\(\\ with respect to the \
Young tableau \)\)\*StyleBox[\"tab\", \"TI\"] . \), DisplayForm]\)"; 
YoungTableauQ::usage = "\!\(\*TagBox[\(YoungTableauQ[ \*StyleBox[\"tab\", \
\"TI\"]] returns True if  \*StyleBox[\"tab\", \"TI\"]\(\(\\ is a proper Young \
tableau, and False otherwise.\)\)\), DisplayForm]\)"; 
$AutoSymmetrizeCovDs::usage = "$AutoSymmetrizeCovDs is a boolean variable \
determining whether all symmetrizable covariant derivatives are automatically \
symmetrized or not. The default is False."; 
$SymCovDCache::usage = 
  "$SymCovDCache stores the cache used by SymmetrizeCovDs."; 
$TensorWrapperColor::usage = "$TensorWrapperColor is a global variable \
specifying the color of the parentheses surrounding the formatting of a \
TensorWrapper expression."; 
