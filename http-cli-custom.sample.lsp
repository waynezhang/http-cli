(define API_KEY "fa5101b5f64c73f552eb1a765c78ba7b")

(define (search-shows key)
  (let (path (string "search/shows.json/" API_KEY "?query=" key))
   (http-cli:run-cmd "get" path)))

(define (show-info sid)
  (let (path (string "show/summary.json/" API_KEY "/" sid))
   (http-cli:run-cmd "get" path)))

(http-cli:register-cmd "s" "search tv shows" search-shows)
(http-cli:register-cmd "i" "show tv show info" show-info)

(http-cli:run-cmd "h" "http://api.trakt.tv")
