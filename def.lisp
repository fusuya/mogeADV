(defparameter *window-w* 640)
(defparameter *window-h* 480)
(defparameter *font-name* "Sans")
(defparameter *font-size* 18)
(defparameter *font-window* nil)
(defparameter *endroll* t)
(defparameter *OP* t)

(if (equal "Linux" (software-type))
    (setf *font-name* "Sans")
    (setf *font-name* "MSゴシック"))

(defparameter *font16* (concatenate 'string *font-name* " " "16"))
(defparameter *font18* (concatenate 'string *font-name* " " "18"))
(defparameter *font20* (concatenate 'string *font-name* " " "20"))
(defparameter *font22* (concatenate 'string *font-name* " " "22"))
(defparameter *font24* (concatenate 'string *font-name* " " "24"))

(defstruct player
  (mogep 0) ;;モゲポイント
  (game 0) ;;op:
  ;;能力
  (str 0)
  (con 0)
  (siz 0)
  (dex 0)
  (app 0)
  (san 0)
  (int 0)
  (pow 0)
  (edu 0)
  (idea 0)
  (luck 0)
  (chisiki 0)
  ;;技能
  (skill '(("目星"       25)
	   ("聞き耳"     25)
	   ("図書館"     25)
	   ("経理"       10)
	   ("応急手当"   10)
	   ("オカルト"    5)
	   ("ナビゲート" 10)
	   ("値切り"      5)
	   ("化学"        1)
	   ("アイデア"   10)
	   ("幸運"       10)
	   ("知識"       10)
	   ("信用"       15)
	   ("言いくるめ"  5)
	   ("地質学"      1)
	   ("博物学"     10)
	   ("心理学"      5)
	   ))
  )


(defparameter *init-skill-point*
  '(
    ("目星"       25)
    ("聞き耳"     25)
    ("図書館"     25)
    ("経理"       10)
    ("応急手当"   10)
    ("オカルト"    5)
    ("ナビゲート" 10)
    ("値切り"      5)
    ("化学"        1)
    ("アイデア"   10)
    ("幸運"       10)
    ("知識"       10)
    ("信用"       15)
    ("言いくるめ"  5)
    ("地質学"      1)
    ("博物学"     10)
    ("心理学"      5)
    ))


(defparameter *end-text*
  '("原案:もげぞう"
    "シナリオ:もげぞう"
    "演出:もげぞう"
    "主演:もげぞう"
    "撮影:もげぞう"
    "編集:もげぞう"
    "プログラム:もげぞう、予定地"
    "監督:もげぞう"))
