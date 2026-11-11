;;(ql:quickload "cl-collider")
(asdf:load-system "cl-collider")

(in-package :sc-user)
(named-readtables:in-readtable :sc)

(setf *s* (make-external-server "127.0.0.1"
                                :port 44556
                                :server-options (make-server-options :num-input-bus 0)))
(server-boot *s*)


;;(jack-connect)


(server-quit *s*)


(proxy
 :sinesynth
 (with-controls ((lfo-speed 4))
   (sin-osc.ar (* [440 441]
                  (range (lf-pulse.ar [lfo-speed (+ lfo-speed .2)]) 0 1))
               0 .2))
 :fade 5.0)


(ctrl :sinesynth :lfo-speed 50)

(ctrl :sinesynth :gate 0)



(proxy
 :sinesynth
 (with-controls ((lfo 5)
                 (speed 2))
   (sin-osc.ar [ (+ (sin-osc.kr speed 0 lfo) 215) 238] 0 0.2))
 :fade 3)


(ctrl :sinesynth :lfo 100)
(ctrl :sinesynth :speed 0.05)


(defsynth plink ((note 60) (atk 0.1) (rel 1) (amp 0.4))
  (let* ((env (env-gen.kr (env-perc atk rel) :act :free))
         (freq (midicps note))
         (sig (* env (lpf.ar (saw.ar freq amp) (* 2 freq) (+ 0.5 env)))))
    (out.ar 0 [sig sig])))


(synth 'plink :note 35)


(defun testing ()
  (let ((syn (synth 'plink
                    :note (+ 40 (random 20))
                    :atk (+ 0.01 (random 0.08))
                    :release (+ 0.3 (random 0.7))))
        (time (+ 0.3 (random 0.6))))
    (callback (+ (now) time) #'release syn)
    (callback (+ (now) (+ time 0.1)) #'testing)))



(server-free-all)
