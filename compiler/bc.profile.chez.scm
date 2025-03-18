(parameterize ([compile-profile #t])
(load "bc.chez.scm"))
(profile-dump-html)
