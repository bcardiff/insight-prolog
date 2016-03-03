(ns insight-prolog.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [insight-prolog.test]))

(doo-tests 'insight-prolog.test)
