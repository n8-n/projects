s.quit;


s.plotTree;
s.meter;


(
SynthDef.new(\blip, { |out, fund=300, dens=2, decay=0.2 |
	var freq, trig, sig;

	freq = LFNoise0.kr(3).exprange(fund, fund*4).round(fund);
	sig = SinOsc.ar(freq) * 0.25;
	trig = Dust.kr(dens);
	sig = sig * EnvGen.kr(Env.perc(0.01, decay), trig);
	sig = Pan2.ar(sig, LFNoise1.kr(10));
	Out.ar(out, sig);
}).add;

SynthDef.new(\verb, { |in, out=0 |
	var sig;

	sig = In.ar(in, 2);
	sig = FreeVerb.ar(sig, 0.5, 0.8, 0.2);
	Out.ar(out, sig);
}).add;

)

(
~reverbBus = Bus.audio(s, 2);
~sourceGroup = Group.new;
~fxGroup = Group.after(~sourceGroup);


// x = Synth.new(\blip, [\out, ~reverbBus], ~sourceGroup);
y = Synth.new(\verb, [\in, ~reverbBus], ~fxGroup);
)

s.freeAll;
~sourceGroup.freeAll;


(
8.do {
	Synth.new(\blip, [\out, ~reverbBus,
		\fund, exprand(60, 420).round(30),
		\decay, exprand(0.1, 0.8),
		\dens, exprand(0.2, 2)
	],
	~sourceGroup);
}
)


~sourceGroup.set(\fund, 110);
~sourceGroup.set(\decay, 5);
~sourceGroup.set(\dens, 1.2);




