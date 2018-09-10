
;;"フォント名 サイズ"
(defun make-font-string ()
  (concatenate 'string *font-name* " " (write-to-string *font-size*)))
(defun make-font-size (size)
  (concatenate 'string *font-name* " " (write-to-string size)))

;;サイコロ 槍マン参上！
(defun dice-100 (f dice me skill-p jump-func succ fail)
  (let* ((num 1) ;;サイコロの出目
         (f2 (make-instance 'frame :master f))
         (l3 (make-instance 'label :master f2 :font *font22*))
         (ok-btn (make-instance 'button :master f2 :text "OK")))
    (pack f2)
    (loop for i from 0 below dice
	        do (let ((l1 (make-instance 'label :master f2 :font *font22*))
		               (l2 (make-instance 'label :master f2 :text (format nil "目標値:~d" skill-p)
					                                   :font *font22*))
		               (l3 (make-instance 'label :master f2 :text "目標値以下で成功"
				                                     :font (make-font-size 8))))
               (pack (list l3 l2 l1) :pady 4)
               (loop for i from 0 to 20 ;;出目くるくる
                     do (setf num (1+ (random me)))
                        (setf (text l1) (write-to-string num))
                        (ltk:process-events)
                        (sleep 0.1))))
    (if (>= skill-p num)
	      (setf (text l3) "成功！")
	      (setf (text l3) "失敗！"))
    (setf (command ok-btn)
          (lambda ()
            (destroy f2)
            (if (>= skill-p num)
                (funcall jump-func succ)
                (funcall jump-func fail))
            (destroy f)))
    (pack (list l3 ok-btn))
    (process-events)))

;;技能選択ボタン
(defun make-skill-button (p f skills jump-func)
  (let* ((f1 (make-instance 'frame :master f)))
    (pack f1)
    (loop for s in skills ;;スキルボタン
	        do (destructuring-bind
                 (skill succ fail)
                 s
               (let* ((skill-p (cadr (assoc skill (player-skill p) :test #'equal))) ;;目標値
                      (btn (make-instance 'button :master f1
                                                  :text (format nil "~a:~d" skill skill-p))))
                 (setf (command btn)
                       (lambda ()
                         (destroy f1)
                         (dice-100 f 1 100 skill-p jump-func succ fail)))
                 (pack btn))))))

;;ゲーム初期化&再スタート
(defun game-init (p)
  (loop for ps in (player-skill p)
	      for ini in *init-skill-point*
	      do (setf (cadr ps) (cadr ini)
                 (player-mogep p) 0)))

;;スピンボックス内の技能ポイント初期化
(defun init-skill-num (p lst)
  (loop for skill in (player-skill p)
	      for num in lst
	      do (setf (text num) (write-to-string (cadr skill)))))

;;技能ポイントセット
(defun set-skill-point (p lst)
  (loop for skill in (player-skill p)
	      for num in lst
	      do (setf (cadr skill) (parse-integer (text num)))))

;;技能ポイント設定画面
(defun setting-skill-point (p f jump-func)
  (let* ((btn nil)
         (ok! t)
         (defo-num (loop for x in (player-skill p)
                         sum (cadr x)))
	       (point 200)
         (f1 (make-instance 'frame :master f))
         (f2 (make-instance 'frame :master f1))
         (l1 (make-instance 'label :master f1
                                   :text (format nil "好きな技能に初期値〜99の間で200ポイント分振り分けてください~%(初期値未満にはできません)")
                                   :font *font16* :wraplength *window-w*))
         (l2 (make-instance 'label :master f1 :text (format nil "残り~dポイント" point)
                                   :font *font16*))
	       (ok-btn (make-instance 'button :master f1 :text "OK")))
    (pack (list f1 l1 l2 f2) :pady  10)
    (loop for s in (player-skill p)
	        for i from 0
	        do (let ((l1 (make-instance 'label :text (car s) :master f2 :font *font16*))
                   (t1 (make-instance 'spinbox :from (cadr s) :to 99 :increment 1 :font *font16*
					                                     :text (format nil "~d" (cadr s))
					                                     :master f2 :width 3)))
               (push t1 btn)
               (multiple-value-bind (r c)
                   (floor i 3)
                 (grid l1 r (* c 2))
                 (grid t1 r (1+ (* c 2))))))

    (setf (command ok-btn)
	        (lambda ()
	          (let ((num-list (reverse (mapcar #'(lambda (x) (parse-integer (text x) :junk-allowed t)) btn))))
	            (if (member nil num-list)
		              (progn (setf (text l1) "数字を入力してください")
			                   (init-skill-num p (reverse btn)))
		              (let ((hoge nil) (old-sum 0) (new-sum 0))
                    (loop for skill in (player-skill p)
			                    for new-p in num-list
			                    do (if (or (> (cadr skill) new-p)
                                     (>= new-p 100))
				                         (progn (setf hoge (car skill))
					                              (return))
				                         (progn (incf old-sum (cadr skill))
					                              (incf new-sum new-p))))
                    (if hoge
                        (setf (text l1) (format nil "[~a]が初期値未満or100以上になってますた" hoge))
                        (if (> (- new-sum old-sum) 200)
                            (setf (text l1) "200ポイント以上振り分けています！")
                            (progn (setf ok! nil)
                                   (set-skill-point p (reverse btn))
                                   (funcall jump-func 0) ;;最初から
                                   (destroy f)))))))))

    (pack ok-btn)
    (loop while ok!
          do (if (not (or (find "" btn :test #'equal :key #'(lambda (x) (text x)))
                          (member nil (mapcar #'(lambda (x) (parse-integer (text x) :junk-allowed t)) btn))))
                 (setf (text l2) (format nil "残り~dポイント" (- 200 (- (apply #'+ (mapcar #'(lambda (x) (parse-integer (text x) :junk-allowed t)) btn)) defo-num)))))
             (sleep 0.1)
             (process-events))))

(defun endroll2 (p f l1 lf2 jump-func)
  (let* ((btn-frame (make-instance 'labelframe :master f :text "選択肢"))
         (skill-frame (make-instance 'frame :master f))
         (re-btn (make-instance 'button :master btn-frame :text "もう一度"))
         (end-btn (make-instance 'button :master btn-frame :text "終わる"))
         (label-list nil) (x (floor (window-width *tk*) 2))
         (text-num (length *end-text*))
         (mozi-w 26) (endroll t))
    (pack btn-frame :before lf2)
    (pack skill-frame :before lf2)
    (pack (list re-btn end-btn))
    (pack-forget l1)
    (setf (command re-btn)
          (lambda ()
            (destroy btn-frame)
            (setf endroll nil)
            (dolist (l label-list)
              (destroy (car l)))
            (game-init p) ;;ポイント初期化
            (pack l1 :padx 5 :pady 5 :fill :both :expand t)
            (setf (text l1) "")
            (setting-skill-point p skill-frame jump-func)))
    (setf (command end-btn)
          (lambda ()
            (setf endroll nil
                  *exit-mainloop* t)))

    (loop while endroll ;;エンドロール表示
          do (when (and (> text-num (length label-list))
                        (or (null label-list) (> (- (window-height *tk*) 100) (cadr (car label-list)))))
               (push (list (make-instance 'label :text (format nil "~a" (nth (length label-list) *end-text*))
                                                 :font *font18* :master lf2)
                           (window-height *tk*)
                           (- x (* mozi-w (floor (length (nth (length label-list) *end-text*)) 2))))
                     label-list))
             (dolist (l label-list)
               (decf (cadr l) 2)
               (place-forget (car l))
               (when (> (cadr l) 0)
                 (place (car l) (caddr l) (cadr l))))
             (sleep 0.05)
             (process-events))))

;;640 26
;;エンドロール
(defun endroll (lf2)
  (let ((label-list nil) (x (floor (window-width *tk*) 2))
        (text-num (length *end-text*))
        (mozi-w 26)) ;;font18のときの文字の幅
    (loop while *endroll*
          do (when (and (> text-num (length label-list))
                        (or (null label-list) (> (- (window-height *tk*) 100) (cadr (car label-list)))))
               (push (list (make-instance 'label :text (format nil "~a" (nth (length label-list) *end-text*))
                                                 :font *font18* :master lf2)
                           (window-height *tk*)
                           (- x (* mozi-w (floor (length (nth (length label-list) *end-text*)) 2))))
                     label-list))
             (dolist (l label-list)
               (decf (cadr l) 2)
               (place-forget (car l))
               (when (> (cadr l) 0)
                 (place (car l) (caddr l) (cadr l))))
             (sleep 0.05)
             (process-events))
    (setf *endroll* t)
    (dolist (l label-list)
      (destroy (car l)))
    (process-events)))

;;ゲーム本編
(defun game-start (p f)
  (let* ((f1 (make-instance 'frame :master f))
         (lf1 (make-instance 'labelframe :master f1 :text "選択肢"))
         (lf2 (make-instance 'frame :master f1 :relief :raised
                                    :borderwidth 4)) ;;:text "テキスト"))
         (l1 (make-instance 'label :master lf2 :width 640
                                   :wraplength *window-w* :anchor :nw
                                   :font *font18*))
         (ip 0)
         (curr-op nil)
         (curr-args nil))

    (pack f1 :fill :both :expand t)
    (pack lf1)
    (pack lf2 :fill :both :expand t)
    (pack l1 :padx 5 :pady 5 :fill :both :expand t)

    (bind *tk* "<Configure>" ;;ウィンドウサイズ変更
          (lambda (e)
            (declare (ignore e))
            (setf *window-w* (window-width *tk*)
                  *window-h* (window-height *tk*))
            (configure l1 :wraplength (window-width l1))))
    (bind l1 "<4>" ;;マウスホイール↑
          (lambda (e)
            (declare (ignore e))
            (incf *font-size*)
            (configure l1 :font (make-font-string))))
    (bind l1 "<5>" ;;マウスホイール↓
          (lambda (e)
            (declare (ignore e))
            (decf *font-size*)
            (configure l1 :font (make-font-string))))

    (labels ((execute ()
               (let ((inst (nth ip *text*)))
                 (cond
                   ((integerp inst) (goto-next))
                   ((null inst) (error "null instruction"))
                   ((listp inst)
                    (setf curr-op (car inst) curr-args (cdr inst))
                    (case curr-op
                      (text
                       (show-next-line))
                      (ask
                       (show-choices))
                      (skill
                       (show-skills))
                      (mogep
                       (update-mogep (car curr-args)))
                      (next
                       (jump (car curr-args)))
                      (tp
                       (destructuring-bind
                           (threshold not-less-label less-label) curr-args
                         (if (>= (player-mogep p) threshold)
                             (jump not-less-label)
                             (jump less-label))))
                      (end
                       (restart-or-end))
                      ;; (let ((btn-frame (make-instance 'frame :master f)))
                      ;;   (pack btn-frame :before lf2)
                      ;;   (ending p btn-frame #'jump)))
                      (t
                       (error "unknown op"))))
                   (t
                    (error "illegal instruction")))))
             (restart-or-end () ;;todo
               (endroll2 p f l1 lf2 #'jump))
             ;; (let* ((btn-frame (make-instance 'labelframe :master f :text "選択肢"))
             ;;        (skill-frame (make-instance 'frame :master f))
             ;;        (re-btn (make-instance 'button :master btn-frame :text "もう一度"))
             ;;        (end-btn (make-instance 'button :master btn-frame :text "終わる")))
             ;;   (pack btn-frame :before lf2)
             ;;   (pack skill-frame :before lf2)
             ;;   (pack (list re-btn end-btn))
             ;;   (pack-forget l1)
             ;;   (setf (command re-btn)
             ;;         (lambda ()
             ;;           (destroy btn-frame)
             ;;           (setf *endroll* nil)
             ;;           (game-init p) ;;ポイント初期化
             ;;           (pack l1 :padx 5 :pady 5 :fill :both :expand t)
             ;;           (setf (text l1) "")
             ;;           (setting-skill-point p skill-frame #'jump)))
             ;;   (setf (command end-btn)
             ;;         (lambda ()
             ;;           (setf *endroll* nil)
             ;;           (setf *exit-mainloop* t)))
             ;;   (endroll lf2)))
             (show-next-line ()
               (when (not (eq 'text curr-op)) (error "not executing text"))
               (if (null curr-args)
                   (goto-next)
                   (setf (text l1) (car curr-args)
                         curr-args (cdr curr-args))))

             (show-choices ()
               (when (not (eq 'ask curr-op)) (error "not executing ask"))
               (let ((btn-frame (make-instance 'labelframe :master f :text "選択肢")))
                 (pack btn-frame :before lf2)
                 (loop for pair in curr-args ;; (string label)
                       do
                          (let ((btn (make-instance 'button :master btn-frame :text (car pair)))
                                (label (cadr pair)))
                            (setf (command btn)
                                  (lambda ()
                                    (jump label)
                                    (destroy btn-frame)))
                            (pack btn)))))

             (show-skills ()
               (when (not (eq 'skill curr-op)) (error "not executing skill"))
               (let ((btn-frame (make-instance 'frame :master f)))
                 (pack btn-frame :before lf2)
                 (make-skill-button p btn-frame curr-args #'jump)))

             (update-mogep (diff)
               (incf (player-mogep p) diff)
               (goto-next))

             (jump (label)
               (let ((index (position label *text*)))
                 (when (not index)
                   (error "jump to nonexistent label"))
                 (setf ip index)
                 (execute)))

             (goto-next ()
               (incf ip)
               (execute)))

      (bind l1 "<Button-1>"
            (lambda (e) (declare (ignore e))
              (case curr-op
                (text
                 (show-next-line))
                (t nil))))
      (let ((skill-frame (make-instance 'frame :master f)))
        (pack skill-frame :before lf2)
        (setting-skill-point p skill-frame #'jump)) ;;技能ポイント設定
      ;;(execute)
      )))





;;スタート画面
(defun start-gamen (p f)
  (let* ((f1 (make-instance 'frame :master f))
         (b1 (make-instance 'button :master f1 :text "スタート"))
         (b2 (make-instance 'button :master f1 :text "終わる")))
    (pack (list f1 b1 b2) :pady 10)
    (setf (command b1)
          (lambda ()
	          (destroy f1)
            (game-start p f))) ;;技能設定
    (setf (command b2)
	        (lambda ()
	          (setf *exit-mainloop* t)))))

(defun hoge ()
  (with-ltk ()
    (wm-title *tk* "ほげ！")
    (bind *tk* "<q>" (lambda (e) (declare (ignore e))
		       (return-from hoge)))
    (set-geometry *tk* *window-w* *window-h* 100 100)
    (let* ((p (make-player))
	         (f (make-instance 'frame)))
      (pack f :fill :both :expand t)
      (start-gamen p f))))
