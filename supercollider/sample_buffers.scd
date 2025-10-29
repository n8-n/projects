s.quit;

~dings = Array.new;
~folder = PathName.new("C:/projects/supercollider/samples");

(
~folder.entries.do({
	arg path;
	~dings = ~dings.add(Buffer.read(s, path.fullPath));
});
)


~dings[4].query;



(
SynthDef.new(\playbuf, {
	arg amp=1, out=0, buf, rate=1, loop=1, start=0, da=2;
	var sig;
	sig = PlayBuf.ar(2, buf, BufRateScale.kr(buf) * rate, loop: loop, doneAction: da);
	sig = sig * amp;
	Out.ar(out, sig);

}).add;
)



x = Synth.new(\playbuf, [\buf, ~dings[0].bufnum]);
x.set(\rate, -7.midiratio);

x.free;


Synth.new(\playbuf, [\buf, ~dings[0].bufnum]);
Synth.new(\playbuf, [\buf, ~dings[0].bufnum, \rate, -7.midiratio]);
Synth.new(\playbuf, [\buf, ~dings[0].bufnum, \rate, -12.midiratio]);


g = Group.new;

(
x = Synth.new(\playbuf, [\buf, ~dings[0].bufnum, \amp, 0.5, \rate, 0.7], g);
y = Synth.new(\playbuf, [\buf, ~dings[0].bufnum, \amp, 0.5, \rate, 0.5], g);
z = Synth.new(\playbuf, [\buf, ~dings[0].bufnum, \amp, 0.5, \rate, 0.2], g);
)

y.set(\rate, -0.15);























