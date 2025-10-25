s.quit;

s.plotTree;


(
/*~tempo_bus = Bus.control(s, 1);

SynthDef(\tempo, {
	Out.kr(~tempo_bus, Impulse.kr(4));
}).add;*/

SynthDef(\pulse, {
	arg freq=300, atk=0.1, rel=0.5;
	var sig, env, pwm, seq, trig, deg;
	// env = Env.perc(atk, rel).kr(Done.freeSelf);
	pwm = SinOsc.kr(3).linlin(0,1,0.3,0.7);

	trig = Impulse.ar(2);
	seq = Dseq([50, 48, 52, 53], inf);
	deg = Duty.ar(Drand([1/4, 1/8, 1/16], inf), 0, seq);
	sig = Pulse.ar(deg.midicps, pwm)!2;
	sig = MoogFF.ar(sig, LFNoise1.kr(2).linlin(0,1,1500,2500), 3.5) * 1.2;
	// sig = sig * env;
	Out.ar(0, sig);
}).add;

SynthDef(\noise, {
	var sig, tempo;
	sig = WhiteNoise.ar(
		Env.perc(0.01, 0.3).kr(
			Done.freeSelf, gate: Impulse.kr(4))
	);
	sig = Pan2.ar(sig, LFNoise1.kr(1).range(-0.5, 0.5)) * 0.8;
	Out.ar(0, sig);
}).add;

)

(
Synth(\pulse);
Synth(\noise);
)