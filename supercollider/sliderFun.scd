
(
// Based on s.volume.gui
~slider = { | initValue=0.5, min=0, max=1, warp=\lin, name="default" |
	var slider, box, simpleController;

	// Bus will be returned as Ugen for plugging into synths.
	// model is used for display and to detect UI changes.
	var bus = Bus.control(s, 1).set(initValue);
	var model = Ref(model);

	var spec = ControlSpec(min, max, warp);

	var bounds = Rect(100, 100, 70, 360);
	var window = Window.new(name, bounds).front;
	var text = StaticText(window, Rect(10, 10, 50, 20));
	text.string = name;

	box = NumberBox(window, Rect(10, 40, 50, 30))
	.value_(initValue);

	slider = Slider(window, Rect(10, 70, 50, 270))
	.value_(spec.unmap(initValue));

	slider.action_({ | item |
		var mapped = spec.map(item.value);
		model
		.value_(mapped)
		.changed(\update, mapped);
	});

	box.action_({ | item |
		model
		.value_(item.value)
		.changed(\update, item.value);
	});


	window.onClose_({
		simpleController.remove;
		bus.free(true);
	});

	simpleController = SimpleController(model)
	.put(\update, {|changer, what, val|
		bus.setSynchronous(val);

		box.value_(val.round(0.01));
		slider.value_(spec.unmap(val));
	});

	// Return bus
	bus.ar
};

)




{~slider.(1, 1, 200, \exp, "LFO")}.play;


{ SinOsc.ar(~slider.(100, 1, 500, \exp, "freq") !2)  * SinOsc.ar(2) *  0.3 }.play;

{ SinOsc.ar(200)!2  * SinOsc.ar(2) *  0.3 }.play;

(
{
    var lfo = VarSaw.ar(~slider.(0.01, 0.01, 10, \lin, "LFO"));
    var freq = ~slider.(200, 50, 500, \exp, "freq");
    LFPulse.ar(freq)!2 * lfo * 0.5
}.play;

)


