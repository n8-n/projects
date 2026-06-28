(
SynthDef(\fm, {
	arg carHz=300, modHz=200, modAmp=200, atk=0.1,
	rel=1, amp=0.4, pan=0;
	var car, mod, env;
	env = Env.perc(atk, rel).kr(doneAction: 2);
	mod = SinOsc.ar(modHz, mul: modAmp);
	car = SinOsc.ar(carHz + mod) * env * amp;
	car = Pan2.ar(car, pan);
	Out.ar(0, car);
}).add;


// index = modAmp/modHz
SynthDef(\fm2, {
	arg freq=440, mRatio=1, cRatio=1, index=1, iScale=5,
	atk=0.1, cAtk=4, cRel=(-4), rel=1, amp=0.4, pan=0;
	var car, mod, env, iEnv;

	iEnv = EnvGen.kr(
		Env.new([index, index * iScale, index],
			[atk, rel], [cAtk, cRel]
	));
	env = Env.perc(atk, rel, curve:[cAtk, cRel]).kr(doneAction: 2);
	mod = SinOsc.ar(freq * mRatio, mul: freq * mRatio * iEnv);
	car = SinOsc.ar(freq * cRatio + mod) * env * amp;
	car = Pan2.ar(car, pan);
	Out.ar(0, car);
}).add;

)


Synth(\fm2, [\freq, 500, \mRatio, 1, \index, 2, \iScale, 5]);





(
p = Pbind(
	\instrument, \fm,
	\dur, 1/8,
	\carHz, Pexprand(20, 10000),
	\modHz, Pexprand(20, 10000),
	\modAmp, Pwhite(0, 10000),
	\amp, Pexprand(0.1, 0.5),
	\atk, Pexprand(0.001, 0.05),
	\rel, Pexprand(0.08, 1.5),
	\pan, Pwhite(-1.0, 1.0)
).play;

)