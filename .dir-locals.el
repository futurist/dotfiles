(
 (nil . (
         (tab-width . 2)
         (eval . (progn
                   (message "set local vars %s" (buffer-file-name) )
                   (add-to-list 'browse-url-filename-alist
                                `(,(file-name-directory (symbol-name (car (car dir-locals-class-alist)))) . "http://localhost:9778/"))
                  ))
         ))
 )
