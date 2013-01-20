Test[
	LevelSpecQ[RandomInteger[{0,1000}]]
	,
	True
	,
	TestID->"xCore-20130101-L6P6S7"
]

Test[
	LevelSpecQ[-RandomInteger[{0,1000}]]
	,
	False
	,
	TestID->"xCore-20130101-J1P6A0"
]

Test[
	LevelSpecQ[{2,3}]
	,
	True
	,
	TestID->"xCore-20130101-L4R7Z0"
]

Test[
	LevelSpecQ[{3,2}]
	,
	False
	,
	TestID->"xCore-20130101-L1Y5R4"
]

data = Table[RandomInteger[{1000, 2000}], {50}]

Test[
	MapTimed[Identity, data]
	,
	data
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-C7F2A8"
]

Test[
	MapTimed[PartitionsQ, data]
	,
	Map[PartitionsQ, data]
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-S2Y1A0"
]

Test[
	MapTimed[nonExistingFunction, data]
	,
	Map[nonExistingFunction, data]
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-Q9R1S7"
]

Test[
	MapTimed[Identity, data, Parallelize->True]
	,
	data
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-C7F2A8"
]

Test[
	MapTimed[PartitionsQ, data, Parallelize->True]
	,
	Map[PartitionsQ, data]
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-S2Y1A0"
]

Test[
	MapTimed[nonExistingFunction, data, Parallelize->True]
	,
	Map[nonExistingFunction, data]
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-Q9R1S7"
]



data = Array[RandomInteger[{0,100}], {3,4,5,7}]

Test[
	MapTimed[PartitionsQ, data, {4}]
	,
	Map[PartitionsQ, data, {4}]
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-E1P8E5"
]

Test[
	MapTimed[PartitionsQ, data, {4}, Parallelize->True]
	,
	Map[PartitionsQ, data, {4}]
	,
	{FrontEndObject::notavail}
	,
	TestID->"xCore-20130101-E1P8E5"
]