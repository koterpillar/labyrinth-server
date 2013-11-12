{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} TestLabyrinthServer.JSON

main = htfMain htf_importedTests
