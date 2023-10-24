module ObserveOtherModule where

import           Data.Set             (Set)
import           ObserveOtherModule2
import           TransitiveAnns.Types

observeAnn :: Set Annotation
observeAnn = annotated otherRefAnn

observeAnn' :: Set Annotation
observeAnn' = annotated otherRefAnn'

observeAdd :: Set Annotation
observeAdd = annotated otherRefAdd

observeAdd' :: Set Annotation
observeAdd' = annotated otherRefAdd'

