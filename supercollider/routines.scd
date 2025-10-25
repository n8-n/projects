s.quit;
s.plotTree;
s.meter;


(
SynthDef.new(\tri, {
	arg out, freq=400, amp=0.8, atk=0.01, rel=0.1;
	var sig, env;
	env = Env.perc(atk, rel, amp).kr(Done.freeSelf);
	sig = LFTri.ar(freq);
	// sig = Pan2.ar(sig, SinOsc.kr(2));
	sig = sig!2 * env;
	Out.ar(out, sig);
}).add;

r = Routine({
	var delta;
	loop {
		delta = rrand(1,3) * 0.5;
		Synth(\tri, [freq: exprand(200, 800), amp: rrand(0.5,0.9), rel: rrand(0.05,0.5)]);
		delta.wait;
	}
});



)


Synth(\tri, [rel: 3, atk: 0.2, freq: 500]);

r.play;
r.stop;