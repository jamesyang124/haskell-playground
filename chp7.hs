import Data.List
import Data.Map
import qualified Data.Set as Set
import Geometry
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube

{--

The syntax for importing modules in a Haskell script is import <module name>. This must be done before defining any functions, so imports are usually done at the top of the file.

--}

-- | If we wanted to import only the nub and sort functions from Data.List, we'd do this:

{--

import Data.List (nub, sort)

we want to import all the functions from Data.List except the nub function:

import Data.List hiding (nub)

If name confliction(ex: filter function in Prelude module) occurs in current module:

import qualified Data.Map

So we can use Data.Map.filter to avoid this, or give an alias qualified name

import qualified Data.Map as M

M.filter

--}

-- | intersperse, intercalate, transpose,
-- | foldl' and foldl1' are stricter versions of their respective lazy incarnations.

{--

When using lazy folds on really big lists, you might often get a stack overflow error. The culprit for that is that due to the lazy nature of the folds, the accumulator value isn't actually updated as the folding happens.

The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with thunks.

--}


res1 = take 3 $ iterate (++ "haha") "haha"
-- | ["haha","hahahaha","hahahahahaha"]

res2 = splitAt 3 "heyman"
-- | ("hey","man)

res3 = splitAt (-3) "heyman"
-- | ("","heyman")


res4 = "hey" `isPrefixOf` "hey there!"
-- | True

res5 = "y t" `isInfixOf` "hey there!"


res6 = 4 `elemIndex` [1,2,3,4,5,6]
-- | Just 3

res7 = 4 `elemIndices` [1,2,3,4,5,6,4,7,8,9,4]
-- | [3, 6, 10]

res8 = zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
-- | [7,9,8]

res9 = zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
-- | [(2,2,5,2),(3,2,5,2),(3,2,3,2)]

-- | import Data.Map

-- | Map is an list with pairs
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

-- | All the elements in a set are unique

res10 = Set.null Set.empty
-- | True

res11 = Set.null $ Set.fromList [3,4,5,5,4,3]
-- | False

-- | Make a module, see top line import Geometry


-- | Modules can also be given a hierarchical structures. Each module can have a number of sub-modules and they can have sub-modules of their own. Let's section these functions off so that Geometry is a module that has three sub-modules, one for each type of object.

-- | Module name should be the same as folder name.
