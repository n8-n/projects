s.quit;

(
d = Dictionary.new;
d.add(\l ->
	PathName("/home/nathan/music_production/Samples/perc_samples/low").entries.collect({
		arg sf;
		Buffer.read(s, sf.fullPath);
	});
);

d.add(\m ->
	PathName("/home/nathan/music_production/Samples/perc_samples/mid").entries.collect({
		arg sf;
		Buffer.read(s, sf.fullPath);
	});
);

d.add(\h ->
	PathName("/home/nathan/music_production/Samples/perc_samples/high").entries.collect({
		arg sf;
		Buffer.read(s, sf.fullPath);
	});
);

)

d[\h].choose.play;



(
SynthDef.new(\sine, {
	arg freq=400, atk=0.005, rel=0.3, amp=1, pan=0;
	var sig, env;

	sig = SinOsc.ar(freq);
	env = EnvGen.kr(Env.new([0,1,0], [atk, rel],[-1,1]), doneAction:2);
	sig = Pan2.ar(sig, pan, amp);
	sig = sig * env;
	Out.ar(0, sig);

}).add;

SynthDef.new(\bufPlay, {
	arg buf=0, rate=1, amp=1;
	var sig;
	sig = PlayBuf.ar(2, buf, BufRateScale.ir(buf) *  rate, doneAction:2);
	sig = sig * amp;
	Out.ar(0, sig);

}).add;

)


(
~quarter = 1.875; // 60/128 * 4

Pdef(
	\sinepat,
	Pbind(
		\instrument, \sine,
		\dur, Pseq([1/8], inf),
		\stretch, ~quarter,
		\note, Pxrand([0,3,5,9,10,12], inf),
		\gtranspose, -1,
		\atk, 0.08,
		\rel, Pexprand(0.01, 1.5, inf),
		\amp, Pexprand(0.05, 0.5, inf),
		\pan, Pwhite(-0.8, 0.8, inf);
	);
).play(quant:~quarter);

)
(
Pdef(
	\rhythm,
	Pbind(
		\instrument, \bufPlay,
		\dur, Pseq([1/16], inf),
		\stretch, ~quarter,
		\buf, Pseq([
			Prand(d[\l], 1),
			Prand(d[\l]++d[\m]++d[\h], 7),
			Prand(d[\h], 1),
			Prand(d[\l]++d[\m]++d[\h], 7)
		], inf),
		\rate, Pexprand(4,5, inf),
		\amp, Pseq([0.8, Pexprand(0.1, 0.2,7)], inf)
	);
).play(quant:~quarter); //quant_(~quarter);

)

s.plotTree;
s.meter;
p.stop;

Pdef(\sinepat).stop;















