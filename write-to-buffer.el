;;; DOC
;;; CODE

(defconst +WTB-buffer-name+ "*my-messages*"
  "Buffer name where information will be inserted.")

(defvar *WTB-fill-paragraph* t
  "Check if inserted text will be justified")

(defun WTB-create-buffer ()
  "Create and display +WTB-BUFFER-NAME+
All insert from writeToBuffer will be placed into this buffer."
  (let ((buf (get-buffer-create +WTB-buffer-name+)))
    (display-buffer buf)
    buf))

(defun WTB-new-line ()
  "Go to next line in +WTB-BUFFER-NAME+."
  (with-current-buffer (WTB-create-buffer)
    (newline)))

(defun WTB-write (message)
  "Write MESSAGE into +WTB-BUFFER-NAME+."
  (with-current-buffer (WTB-create-buffer)
    (goto-char (point-max))
    (insert (format "%S" message))
    (when *WTB-fill-paragraph*
      (fill-paragraph))))

(defun WTB-write-line (message)
  "Write MESSAGE into +WTB-BUFFER-NAME+ with a newline ending."
  (WTB-write message)
  (WTB-new-line)
  (WTB-new-line))

(defun WTB-clear ()
  "Clear all messages in +WTB-BUFFER-NAME+."
  (with-current-buffer +WTB-buffer-name+
    (erase-buffer)))

;; Exemple of usage:
;; (setq *WTB-fill-paragraph* t)
;; ;; (setq *WTB-fill-paragraph* nil)
;; (let ((var '(symbol
;; 	     (1 2 3 4 5 6)
;; 	     "String"
;; 	     8001
;; 	     3.1415
;; 	     'c
;; 	     "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed id elementum dolor. Aenean mi est, convallis vitae tortor et, tempus tincidunt metus. Vestibulum pulvinar at dui non egestas. Quisque laoreet velit rhoncus leo sagittis, ut iaculis elit mattis.")))
;;   (WTB-create-buffer)
;;   (WTB-clear)
;;   (dolist (elem var)
;;     (WTB-write-line elem)))

(provide 'write-to-buffer)
;;; end of write-to-buffer.el
