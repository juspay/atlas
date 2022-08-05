{- |
Copyright 2022 Juspay Technologies Pvt Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Module      :  Mobility.Fixtures.Routes
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Mobility.Fixtures.Routes where

import Beckn.Types.MapSearch
import Data.List.NonEmpty

type LocationUpdates = NonEmpty (NonEmpty LatLong)

locationUpdatesRoute1 :: LocationUpdates
locationUpdatesRoute1 =
  (LatLong 10.021737163149579 76.30619847435514 :| [])
    :| [ LatLong 10.022594311263232 76.30556874855885
           :| [ LatLong 10.022551155038336 76.30441176779023,
                LatLong 10.022896404676642 76.30350020718463
              ],
         LatLong 10.023494484407422 76.30321503690381
           :| [ LatLong 10.023880128960258 76.30264171106143
              ]
       ]

-- test case from production
locationUpdatesIsolatedPoint :: LocationUpdates
locationUpdatesIsolatedPoint =
  ( LatLong 10.0300607 76.3090286
      :| [ LatLong 10.0300588 76.3090286,
           LatLong 10.0300579 76.3090286,
           LatLong 10.0300645 76.3090362,
           LatLong 10.0300741 76.3090362,
           LatLong 10.0300798 76.3090515,
           LatLong 10.0301017 76.3092269,
           LatLong 10.0301303 76.3095169,
           LatLong 10.0301666 76.3097686,
           LatLong 10.0302333 76.3100738,
           LatLong 10.0304136 76.3100662,
           LatLong 10.0306167 76.3100204,
           LatLong 10.0306625 76.3100128,
           LatLong 10.03092 76.3099746,
           LatLong 10.0312366 76.3099288,
           LatLong 10.0316858 76.3098831,
           LatLong 10.0323753 76.3096618,
           LatLong 10.0327644 76.3094482,
           LatLong 10.033183 76.3091201,
           LatLong 10.0337142 76.3088607,
           LatLong 10.0341749 76.3087081,
           LatLong 10.0347824 76.3083343,
           LatLong 10.0353813 76.3080978,
           LatLong 10.0358829 76.3078231,
           LatLong 10.0363435 76.3073883,
           LatLong 10.0367546 76.3070068,
           LatLong 10.0371685 76.3068161,
           LatLong 10.0372543 76.3067245,
           LatLong 10.0377664 76.3066253,
           LatLong 10.0382642 76.3065109,
           LatLong 10.0384626 76.3064422,
           LatLong 10.0385293 76.3063278,
           LatLong 10.0384836 76.3059844,
           LatLong 10.0384349 76.3055419,
           LatLong 10.0383958 76.3052291,
           LatLong 10.0382919 76.3049468,
           LatLong 10.0382852 76.3046951,
           LatLong 10.0387496 76.304634,
           LatLong 10.039237 76.3045501,
           LatLong 10.0396852 76.3044967,
           LatLong 10.04006 76.3044967,
           LatLong 10.040142 76.3043212,
           LatLong 10.0400123 76.3040542,
           LatLong 10.0399856 76.3036804,
           LatLong 10.0399723 76.3031311,
           LatLong 10.0398044 76.3026275,
           LatLong 10.0395908 76.3023452,
           LatLong 10.0393781 76.3017883,
           LatLong 10.0391864 76.301239,
           LatLong 10.039505 76.3009719,
           LatLong 10.0400228 76.3007125,
           LatLong 10.0405063 76.3004684,
           LatLong 10.0408935 76.3002853,
           LatLong 10.0411548 76.3001785,
           LatLong 10.041522 76.2999954,
           LatLong 10.0418853 76.299797,
           LatLong 10.0420637 76.2997055,
           LatLong 10.0424327 76.2994995,
           LatLong 10.0427255 76.2992935,
           LatLong 10.0432443 76.2990341,
           LatLong 10.0434694 76.2989196,
           LatLong 10.0434722 76.2989273,
           LatLong 10.0434808 76.2989273,
           LatLong 10.0434913 76.2989196,
           LatLong 10.043497 76.298912,
           LatLong 10.0434904 76.2989196,
           LatLong 10.043477 76.2989349,
           LatLong 10.0434703 76.2989501,
           LatLong 10.0434675 76.2989501,
           LatLong 10.0434694 76.2989501,
           LatLong 10.0434761 76.2989501,
           LatLong 10.0434894 76.2989349,
           LatLong 10.0440378 76.2986145,
           LatLong 10.0447063 76.298233,
           LatLong 10.0455675 76.2976455,
           LatLong 10.04636 76.2970581,
           LatLong 10.0471668 76.2964553,
           LatLong 10.0479345 76.2957839,
           LatLong 10.0484142 76.2955474,
           LatLong (-7.1629128) 68.7651748, -- isolated point
           LatLong 10.0495701 76.2949714,
           LatLong 10.0501216 76.2948041,
           LatLong 10.050295 76.29453,
           LatLong 10.0503389 76.2944883,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503883 76.2945033,
           LatLong 10.0503861 76.2945015,
           LatLong 10.0503133 76.294445,
           LatLong 10.0503133 76.294445,
           LatLong 10.0503133 76.294445,
           LatLong 10.0503133 76.294445,
           LatLong 10.0503133 76.294445,
           LatLong 10.0503133 76.294445,
           LatLong 10.0503133 76.294445,
           LatLong 10.0503559 76.2944869,
           LatLong 10.0503559 76.2944793,
           LatLong 10.0503549 76.2944259,
           LatLong 10.0503225 76.2942047,
           LatLong 10.0509157 76.2939376,
           LatLong 10.0517425 76.2935943,
           LatLong 10.0526285 76.2932281,
           LatLong 10.0534992 76.2928619,
           LatLong 10.0543222 76.2925109,
           LatLong 10.0553579 76.2920761,
           LatLong 10.0563602 76.2916412,
           LatLong 10.0574741 76.2910766,
           LatLong 10.0585098 76.2904663,
           LatLong 10.0594377 76.2898864,
           LatLong 10.0602312 76.2894058,
           LatLong 10.0608749 76.2890319,
           LatLong 10.061491 76.2886276,
           LatLong 10.0612716 76.2880706,
           LatLong 10.061016 76.2876281,
           LatLong 10.0610208 76.2876281,
           LatLong 10.0610198 76.2876281,
           LatLong 10.0607271 76.287117,
           LatLong 10.0600461 76.2862014,
           LatLong 10.0589523 76.2852783,
           LatLong 10.0575313 76.2845916,
           LatLong 10.056117 76.2839508,
           LatLong 10.0548315 76.2830581,
           LatLong 10.0538005 76.2819595,
           LatLong 10.0529499 76.2807312,
           LatLong 10.0522375 76.2797393,
           LatLong 10.0516042 76.278778,
           LatLong 10.0511093 76.2779006,
           LatLong 10.0507364 76.2771301,
           LatLong 10.0506124 76.2766723,
           LatLong 10.0505781 76.2766036,
           LatLong 10.0502758 76.2758636,
           LatLong 10.0500659 76.2752914,
           LatLong 10.049839 76.2745666,
           LatLong 10.0496301 76.2738265,
           LatLong 10.0492906 76.2735595,
           LatLong 10.0490865 76.2729949,
           LatLong 10.0486259 76.2730636,
           LatLong 10.0479974 76.2731933,
           LatLong 10.0474138 76.2732772,
           LatLong 10.0467462 76.2733459,
           LatLong 10.0462007 76.2734146,
           LatLong 10.0456361 76.2735214,
           LatLong 10.0449743 76.2736816,
           LatLong 10.044445 76.2738571,
           LatLong 10.0439491 76.2739791,
           LatLong 10.0437736 76.2741546,
           LatLong 10.0438728 76.2745666,
           LatLong 10.0439176 76.275093,
           LatLong 10.0440673 76.2755126,
           LatLong 10.044177 76.2761154,
           LatLong 10.0442771 76.2767028,
           LatLong 10.0442867 76.2771224,
           LatLong 10.0441904 76.2774124,
           LatLong 10.0440778 76.2776412,
           LatLong 10.0440673 76.2777328,
           LatLong 10.0440654 76.2777328,
           LatLong 10.0440645 76.2777404,
           LatLong 10.0440616 76.2777481,
           LatLong 10.0440607 76.2777481,
           LatLong 10.0440587 76.2777557,
           LatLong 10.0440549 76.2777633,
           LatLong 10.0440559 76.2777709,
           LatLong 10.0440559 76.2777709,
           LatLong 10.0440607 76.2777709,
           LatLong 10.0440626 76.2777709,
           LatLong 10.0440559 76.2777709
         ]
  )
    :| []
