// This code was created by pygmsh v5.0.1.
p96 = newp;
Point(p96) = {1814142.368325913, 293202.8682982993, 0.0};
p97 = newp;
Point(p97) = {1812504.134841345, 292763.9157856878, 0.0};
p98 = newp;
Point(p98) = {1813915.0536318822, 293014.7457928944, 0.0};
p99 = newp;
Point(p99) = {1813099.8561084608, 292952.03829109273, 0.0};
l32 = newl;
Spline(l32) = {p96, p98, p98, p99, p97};
p100 = newp;
Point(p100) = {1814259.9448917913, 293124.48392104724, 0.0};
l33 = newl;
Spline(l33) = {p96, p100};
p101 = newp;
Point(p101) = {1816517.4149566507, 290678.89135078294, 0.0};
p102 = newp;
Point(p102) = {1812284.6585850392, 292113.3254544957, 0.0};
p103 = newp;
Point(p103) = {1812653.065158124, 291125.68230111967, 0.0};
p104 = newp;
Point(p104) = {1814103.1761372872, 290490.76884537796, 0.0};
p105 = newp;
Point(p105) = {1815733.57118413, 290459.4150944771, 0.0};
l34 = newl;
Spline(l34) = {p97, p102, p103, p104, p105, p101};
p106 = newp;
Point(p106) = {1816282.2618248945, 291478.41199875396, 0.0};
p107 = newp;
Point(p107) = {1815615.994618252, 292074.13326586963, 0.0};
p108 = newp;
Point(p108) = {1815216.2342942664, 292215.2251449233, 0.0};
p109 = newp;
Point(p109) = {1814612.6745894256, 292466.05515212996, 0.0};
p110 = newp;
Point(p110) = {1814189.3989522643, 292967.71516654314, 0.0};
p111 = newp;
Point(p111) = {1814189.3989522643, 293030.4226683448, 0.0};
l35 = newl;
Spline(l35) = {p101, p106, p107, p108, p109, p110, p111, p100};
ll8 = newll;
Line Loop(ll8) = {l35, -l33, l32, l34};
p112 = newp;
Point(p112) = {1814651.8667780517, 293577.15369967796, 0.0};
p113 = newp;
Point(p113) = {1814581.3208385247, 293643.7804203422, 0.0};
l36 = newl;
Spline(l36) = {p112, p113};
Transfinite Line {l36} = 3 Using Progression 1;
p114 = newp;
Point(p114) = {1814475.5019292345, 293522.2846356015, 0.0};
p115 = newp;
Point(p115) = {1814369.6830199442, 293416.4657263112, 0.0};
p116 = newp;
Point(p116) = {1814263.864110654, 293302.80837929575, 0.0};
p117 = newp;
Point(p117) = {1814189.3989522646, 293236.1816586315, 0.0};
l37 = newl;
Spline(l37) = {p113, p114, p115, p116, p117, p96};
Transfinite Line {l37} = 30 Using Progression 1;
p118 = newp;
Point(p118) = {1814365.7638010816, 293240.1008774941, 0.0};
p119 = newp;
Point(p119) = {1814538.2094310361, 293439.9810394868, 0.0};
l38 = newl;
Spline(l38) = {p100, p118, p119, p112};
Transfinite Line {l38} = 30 Using Progression 1;
l39 = newl;
Spline(l39) = {p96, p100};
Transfinite Line {l39} = 3 Using Progression 1;
ll9 = newll;
Line Loop(ll9) = {l39, l38, l36, l37};
s8 = news;
Plane Surface(s8) = {ll9};
Transfinite Surface {s8};
Recombine Surface {s8};
Physical Surface("s8") = {s8};
s9 = news;
Plane Surface(s9) = {ll8,ll9};
Physical Surface("s9") = {s9};
Physical Curve("coast") = {l32,l35,l37,l38};
Physical Curve("ocean") = {l34};
Physical Curve("river") = {l36};
field1 = newf;
Field[field1] = Structured;
Field[field1].FileName= '/home/remy/Software/QGmesh/test/mix_bnd/scaled.dat';
Mesh.CharacteristicLengthFromPoints = 0;
Mesh.CharacteristicLengthFromCurvature = 0;
Mesh.CharacteristicLengthExtendFromBoundary = 0;
field2 = newf;
Field[field2] = Min;
Field[field2].FieldsList = {field1};
Background Field = field2;
