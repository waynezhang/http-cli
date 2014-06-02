;; @module http-cli.lsp
;; @description make http request more convenience
;; @author Linghua Zhang 2014
;; @version v. 0.1 - initial release

(new Tree 'http-cli-env)
(new Tree 'http-cli-funcs)

(load "init.lsp")
(load "color.lsp")

(context 'http-cli)

;; public

;; @syntax (content-type? <type> <headers>)
;; @params <type> The Content-Type string.
;; @params <headers> HTTP header string.
;; @return boolean
;; @example
;; (setq headers [text]HTTP/1.1 200 OK
;;Content-Type: text/html
;;Server: nginx/1.4.1[/text])
;;
;; (content-type? "text/html" headers) => true
;; (content-type? "application/json" headers) => false
;;
;;
(define (content-type? type headers)
 (let (content-type (first (get-header "Content-Type" headers)))
   (find type content-type)))

;; @syntax (run-cmd <cmd>)
;; @params <cmd> The command to execute.
;; @example
;; (run-cmd "?") => run ? command to show command list
;;
;;
(define (run-cmd cmd) (apply process-cmd (cons cmd (args))))

;; @syntax (register-cmd <cmd> <doc> <func>)
;; @params <cmd> The command.
;; @params <doc> The description of command.
;; @params <func> The function of command.
;; @example
;; (register-cmd "e" "example" (fn () (run-cmd "get" "http://example.com"))) => register a "e" command which gets contents of http://example.com
;;
;;
(define (register-cmd cmd doc func)
  (letn (cmds (http-cli-funcs "cmd"))
    (pop-assoc cmd cmds)
    (push (list cmd doc func) cmds)
    (http-cli-funcs "cmd" cmds)))

;; @syntax (register-hook <type> <priority> <doc> <func>)
;; @params <type> The type of hook. "response" and "request" are currently supported.
;; @params <priority> The eval order of hook.
;; The higher the earlier.
;; Previous hook which has the same priority will be deleted.
;; Used proirity:
;;   Request:
;;     100 Append cookie string to header
;;     101 Not Implentment yet
;;   Response:
;;     100 Save cookie
;;     51  Print json body
;;     50  Print html body
;; @params <func> The function of hook.
;; For "request" hook:
;;   The argument is header string.
;;   Return value will be passed to the next hook.
;;   Return value can not be nil.
;; For "request" hook:
;;   The argument is a list which includes a header string and a body string
;;   Return value will be passed to the next hook.
;;   If nil returned then the hook chain stops. 
;; @example
;;
;;
(define (register-hook type priority doc func)
 (letn (hooks (or (http-cli-funcs (hook-key type)) (list)))
  (pop-assoc priority hooks)
  (push (list priority doc func) hooks)
  (http-cli-funcs (hook-key type) hooks)))

;; intrnal things
;; do not use functions below in custom script

;; envs
(http-cli-env "base_url" "")
(http-cli-env "path" "")
(http-cli-funcs "cmd" (list))

;; utils
;; show prompt with str
(define (prompt str)
  (print color:COLOR_WHITE str color:COLOR_NC)
  (read-line 1))

;; url manipulate
;; concat all arguments with "/" 
(define (concat-path)
  (letn (raw-path (join (args) "/")
         paths (filter (fn (s) (> (length s) 0)) (parse raw-path "/")))
   (join paths "/")))

;; build url with base url and path, no argument received
(define (build-url)
  (if (= (length (http-cli-env "path")))
    (string (http-cli-env "base_url") "/")
    (string (http-cli-env "base_url") "/" (http-cli-env "path"))))

(define (set-base-url url)
 (or
  (nil? url)
  (http-cli-env "base_url" (trim url " " "/"))))

;; concat all arguments to path
(define (push-path)
 (letn (curr-path (http-cli-env "path")
        new-path (apply concat-path (cons curr-path (args))))
   (http-cli-env "path" new-path)))

;; pop one level up
(define (pop-path)
  (let (a (parse (http-cli-env "path") "/"))
    (http-cli-env "path" (join (slice a 0 -1) "/"))))

(define (clear-path) (http-cli-env "path" ""))

;; CNC

;; print all commands
(define (show-cmds)
  (dolist (c (-> (http-cli-funcs "cmd")
              (sort (fn (left right) (< (first left) (first right))))))
    (println (format "%-20s%s%s%s" (first c) color:COLOR_WHITE (c 1) color:COLOR_NC))))

;; execute a command
(define (process-cmd cmd)
  (if (not (nil? cmd))
   (let (cmd (assoc cmd (http-cli-funcs "cmd")))
    (if (nil? cmd)
     (println color:COLOR_WHITE "invalid command, ? for help" color:COLOR_NC)
     (apply (cmd 2) (args))))))

;; hook commands

(define (hook-key type) (string "hook-" type))

;; get hooks with type, sorted by priority desending
(define (get-hooks type)
 (let (hooks (http-cli-funcs (hook-key type)))
  (if (list? hooks)
   (sort hooks (fn (left right) (> (left 0) (right 0))))
   (list))))

;; http request

;; print http header
(define (pretty-http-header header)
  (let (lines (parse header {\r\n?} 0))
   (println color:COLOR_GREEN (first lines) color:COLOR_NC)
   (->> lines
    (rest)
    (map (fn (v) (println (replace {(^.*?):(.*)} v (string $1 ":" color:COLOR_WHITE $2 color:COLOR_NC) 0)))))))

;; take request / response through hook chain
(define (process-result type value)
  (letn (func (fn (value hooks)
               (cond
                ((nil? value) nil) 
                ((> (length hooks) 0) (let (processed (apply (hooks (list 0 2)) (list value)))
                  (func processed (rest hooks))))
                (true value))))
   (func value (get-hooks type))))

;; make a request with method and path
(define (make-request method path)
  (letn (p (concat-path (http-cli-env "path") (or path ""))
         url (string (http-cli-env "base_url") "/" p)
         header (process-result "request" "")
         fname (string "make-" method "-request")
         resp (apply (sym fname) (list url header))
         idx (ref "" resp))
   (if (not (nil? idx))
    (let (header (join (slice resp 0 (idx 0)) "\r\n")
          body (join (slice resp (inc (idx 0))) "\r\n"))
     (process-result "response" (list header body))))))

;; return value of header
(define (get-header name headers)
 (letn (lowercased (lower-case name)
        result (->> (parse headers "\n")
          (map (fn (v) (let (parsed (->
                                     (parse v ":")
                                     (extend (list "" ""))
                                     (slice 0 2)))
                        (list (lower-case (parsed 0)) (trim (parsed 1))))))
          (filter (fn (kv) (= lowercased (first kv))))))
  (if (not (empty? result))
   (map last result)
   nil)))

;; GET
(define (make-get-request url header)
 (exec (format "curl -i --silent %s --header '%s'" url header)))

;; POST
(define (make-post-request url header)
 (letn (body (build-post-body)
        resp (exec (format "curl -i --silent %s -X POST --header '%s' -d \"%s\"" url header body)))
  (clear-post-param) 
  resp))

;; concat all post param
(define (build-post-body)
 (letn (params (or (http-cli-env "post-param") (list))
        concated (map (fn (x) (join x "=")) params))
  (join concated "&")))

(define (add-post-param k v)
  (if (or (nil? k) (nil? v)) (throw-error "invalid parameters"))
  (let (param (or (http-cli-env "post-param") (list)))
    (push (list k v) param)
    (http-cli-env "post-param" param)))

(define (clear-post-param)
  (http-cli-env "post-param" (list)))

(define (show-post-params)
  (let (param (or (http-cli-env "post-param") (list)))
    (dolist (ele param)
      (println (format "%-10s %s%s%s" (ele 0) color:COLOR_WHITE (ele 1) color:COLOR_NC)))))

;; hooks

;; default print hook for result
(define (pretty-result result)
  (pretty-http-header (result 0))
  (println "")
  (println (result 1)) 
  nil)

;; hook to print json, formatted by jsonpp
(define (pretty-result-json result)
  (if (content-type? "application/json" (first result))
      (begin
       (pretty-http-header (result 0))
       (print color:COLOR_PURPLE)
       (exec "jsonpp" (result 1))
       (print color:COLOR_NC)
       (println)
       nil)
      result))

;; hook to append cookies to request
(define (append-cookies header)
  (letn (cookies (or (http-cli-env "cookies") (list))
         cookie-str (string "Cookie: " (join (map (fn (v) (join v "=")) cookies) ", ")))
    (cond
      ((empty? cookies) header)
      ((= 0 (length header)) cookie-str)
      (true (string header "\r\n" cookie-str)))))

;; hook to save cookies
(define (save-cookies result)
 (letn (ignore-heaers (list "secure" "expires" "path" "max-age" "domain" "comment")
        func (fn (v)
          (->> (parse v ";")
           (map (fn (v) (parse (trim v) "=")))
           (filter (fn (v) (= (length v) 2)))
           (filter (fn (v) (not (ref (first v) ignore-heaers))))
           (first)))
        cookies (or (get-header "Set-Cookie" (result 0)) (list)) 
        current (or (http-cli-env "cookies") (list)))
  (dolist (c cookies)
   (letn (pair (func c))
    (pop-assoc (first pair) current)
    (push pair current)))
  (http-cli-env "cookies" current))
 result)

;; show / clear all cookies
(define (show-cookies) (map (fn (v)
                             (println (first v) "=" color:COLOR_WHITE (last v) color:COLOR_NC))
                        (or (http-cli-env "cookies") (list))))
(define (clear-cookies) (http-cli-env "cookies" nil))

(define (append-custom-headers headers) headers)

(register-hook "request" 100 "add cookie" append-cookies)
(register-hook "request" 101 "build custom headers" append-custom-headers)

(register-hook "response" 100 "save cookie" save-cookies)
(register-hook "response" 51 "print json reponse" pretty-result-json)
(register-hook "response" 50 "print normal reponse" pretty-result)

;; commands
(register-cmd "get" "make a GET request" (fn () (apply make-request (cons "get" (args)))))
(register-cmd "post" "make a POST request" (fn () (apply make-request (cons "post" (args)))))
(register-cmd "pa" "add a POST parameter" add-post-param)
(register-cmd "pc" "clear all POST parameters" clear-post-param)
(register-cmd "p" "show all POST parameters" show-post-params)
;; cookies
(register-cmd "c" "show cookies" show-cookies)
(register-cmd "cc" "clear cookies" clear-cookies)
;; path commands
(register-cmd "cd" "change path" push-path)
(register-cmd ".." "change to parent path" pop-path)
(register-cmd "/" "change to root path" clear-path)
;; others
(register-cmd "h" "set host (base url)" set-base-url)
(register-cmd "?" "show help page" show-cmds)
(register-cmd "r" "reload custom script" load-custom-script)
(register-cmd "q" "quit" exit)

;; load custom scripts
(if (file? "http-cli-custom.lsp")
 (load "http-cli-custom.lsp"))

(define (load-custom-script)
 (if (file? "http-cli-custom.lsp")
  (load "http-cli-custom.lsp")))

;; start
(define (start)
 (load-custom-script)
 (while true
  (let (cmd (prompt (string (build-url) "> ")))
   (if (string? cmd)
    (or
     (catch (apply process-cmd (parse (trim cmd " " " ") " ")) 'error)
     (let (msg (-> (last-error)
                (last)
                (parse {[:\r\n]} 0)
                (2)
                (first)
                (trim)))
      (println color:COLOR_RED msg color:COLOR_NC)))
    (exit)))))

(context MAIN)

(let (url (last (main-args)))
 (if (regex {http[s]?://.*} url)
  (http-cli:set-base-url url)))

(http-cli:start) 
