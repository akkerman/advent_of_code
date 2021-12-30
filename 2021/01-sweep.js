const example = [199, 200 , 208 , 210 , 200 , 207 , 240 , 269 , 260 , 263]
const sweep = [ 159, 170, 171, 170, 168, 167, 166, 164, 163, 154, 155, 158, 146, 153, 167, 166, 182, 188, 189, 201, 205, 212, 220, 228, 229, 230, 214, 221, 224, 226, 227, 237, 233, 236, 242, 214, 226, 233, 237, 233, 244, 246, 255, 256, 246, 247, 252, 265, 266, 267, 269, 270, 264, 269, 285, 298, 307, 308, 313, 318, 319, 312, 306, 314, 340, 341, 349, 364, 363, 362, 350, 352, 356, 358, 359, 350, 382, 384, 385, 373, 364, 366, 370, 373, 400, 402, 442, 459, 434, 435, 454, 470, 474, 479, 484, 485, 487, 497, 538, 541, 544, 512, 520, 530, 529, 532, 538, 556, 557, 560, 557, 525, 533, 525, 528, 533, 540, 546, 539, 536, 539, 545, 542, 567, 595, 604, 616, 608, 609, 612, 613, 621, 622, 624, 625, 633, 632, 634, 627, 631, 632, 640, 646, 648, 649, 646, 647, 655, 657, 658, 654, 655, 656, 655, 659, 658, 662, 664, 665, 667, 668, 667, 670, 675, 686, 687, 691, 694, 687, 701, 690, 693, 703, 702, 703, 702, 709, 710, 719, 743, 754, 752, 753, 769, 775, 772, 771, 768, 765, 766, 767, 769, 783, 777, 782, 793, 809, 808, 815, 817, 792, 798, 800, 802, 780, 781, 776, 778, 779, 771, 762, 760, 761, 770, 768, 773, 776, 784, 793, 807, 823, 825, 831, 832, 834, 844, 845, 852, 844, 835, 837, 838, 834, 839, 840, 848, 842, 873, 883, 904, 903, 905, 911, 929, 928, 957, 960, 961, 960, 965, 964, 950, 952, 943, 937, 941, 936, 937, 938, 945, 967, 954, 953, 950, 951, 960, 961, 962, 964, 970, 971, 972, 946, 952, 956, 953, 966, 967, 964, 982, 997, 998, 995, 994, 1003, 992, 994, 988, 1005, 1009, 1015, 1016, 1017, 1021, 1019, 1036, 1037, 1038, 1031, 1034, 1035, 1024, 1027, 1049, 1045, 1052, 1012, 1009, 1020, 1031, 1033, 1036, 1032, 1022, 1023, 1024, 1016, 1017, 1021, 1034, 1031, 1011, 1013, 1015, 1016, 1030, 1043, 1044, 1048, 1053, 1054, 1077, 1078, 1080, 1083, 1084, 1083, 1112, 1124, 1125, 1126, 1118, 1117, 1116, 1122, 1125, 1130, 1136, 1140, 1137, 1128, 1157, 1158, 1144, 1146, 1144, 1127, 1145, 1147, 1146, 1125, 1133, 1134, 1133, 1156, 1143, 1160, 1156, 1158, 1160, 1159, 1185, 1201, 1184, 1196, 1198, 1193, 1195, 1204, 1205, 1224, 1235, 1230, 1231, 1229, 1214, 1221, 1225, 1227, 1228, 1223, 1226, 1227, 1239, 1261, 1264, 1265, 1279, 1278, 1271, 1288, 1290, 1288, 1293, 1297, 1300, 1307, 1327, 1332, 1330, 1327, 1304, 1324, 1327, 1328, 1330, 1344, 1330, 1328, 1330, 1331, 1328, 1317, 1320, 1310, 1311, 1322, 1326, 1314, 1313, 1317, 1306, 1312, 1322, 1315, 1318, 1310, 1313, 1314, 1319, 1306, 1302, 1305, 1303, 1305, 1303, 1306, 1303, 1271, 1277, 1283, 1299, 1310, 1328, 1313, 1328, 1332, 1338, 1353, 1346, 1355, 1356, 1353, 1370, 1365, 1367, 1370, 1377, 1374, 1376, 1378, 1386, 1384, 1386, 1387, 1403, 1404, 1405, 1411, 1420, 1421, 1423, 1413, 1411, 1404, 1402, 1413, 1432, 1431, 1446, 1436, 1435, 1432, 1444, 1459, 1482, 1481, 1482, 1481, 1505, 1507, 1511, 1517, 1530, 1531, 1522, 1532, 1538, 1540, 1544, 1546, 1548, 1554, 1555, 1558, 1527, 1529, 1530, 1523, 1508, 1510, 1512, 1509, 1514, 1526, 1527, 1531, 1539, 1570, 1584, 1608, 1637, 1643, 1651, 1660, 1671, 1672, 1674, 1679, 1687, 1685, 1678, 1681, 1692, 1698, 1728, 1734, 1725, 1724, 1751, 1756, 1765, 1775, 1778, 1791, 1793, 1788, 1819, 1824, 1830, 1831, 1830, 1852, 1854, 1844, 1848, 1879, 1898, 1914, 1922, 1927, 1930, 1929, 1943, 1947, 1948, 1965, 1934, 1937, 1939, 1947, 1948, 1944, 1949, 1956, 1954, 1984, 2004, 2008, 2019, 2021, 2022, 2031, 2036, 2054, 2057, 2060, 2061, 2063, 2062, 2063, 2064, 2054, 2058, 2055, 2052, 2057, 2060, 2061, 2060, 2064, 2061, 2063, 2064, 2060, 2061, 2044, 2036, 2040, 2041, 2033, 2020, 2003, 2018, 2017, 2020, 2021, 2013, 2015, 2024, 2041, 2052, 2050, 2075, 2082, 2083, 2084, 2083, 2085, 2091, 2094, 2104, 2107, 2108, 2101, 2103, 2114, 2103, 2107, 2108, 2110, 2104, 2110, 2114, 2123, 2137, 2149, 2155, 2152, 2149, 2150, 2141, 2159, 2162, 2168, 2181, 2182, 2195, 2196, 2202, 2206, 2211, 2221, 2220, 2231, 2238, 2242, 2221, 2211, 2186, 2191, 2168, 2169, 2177, 2178, 2181, 2188, 2214, 2228, 2227, 2228, 2263, 2269, 2268, 2269, 2278, 2279, 2255, 2256, 2257, 2260, 2267, 2260, 2250, 2251, 2250, 2254, 2252, 2248, 2245, 2248, 2250, 2251, 2255, 2257, 2270, 2282, 2286, 2287, 2288, 2313, 2314, 2343, 2338, 2369, 2367, 2369, 2411, 2408, 2389, 2390, 2392, 2378, 2379, 2380, 2382, 2342, 2345, 2343, 2339, 2340, 2341, 2370, 2378, 2395, 2398, 2394, 2382, 2384, 2377, 2384, 2385, 2386, 2387, 2401, 2408, 2409, 2386, 2390, 2405, 2414, 2416, 2420, 2431, 2430, 2446, 2458, 2459, 2467, 2475, 2477, 2481, 2482, 2484, 2485, 2490, 2498, 2502, 2503, 2492, 2487, 2485, 2487, 2474, 2475, 2469, 2472, 2486, 2502, 2504, 2505, 2506, 2504, 2502, 2504, 2503, 2518, 2520, 2535, 2520, 2518, 2521, 2531, 2540, 2545, 2548, 2550, 2551, 2556, 2554, 2558, 2559, 2574, 2571, 2573, 2574, 2582, 2583, 2596, 2602, 2603, 2610, 2618, 2616, 2617, 2629, 2628, 2626, 2641, 2638, 2637, 2643, 2645, 2646, 2652, 2666, 2668, 2671, 2673, 2672, 2675, 2678, 2671, 2670, 2682, 2684, 2667, 2668, 2670, 2667, 2671, 2666, 2664, 2663, 2666, 2663, 2673, 2674, 2675, 2681, 2671, 2675, 2677, 2670, 2669, 2674, 2673, 2682, 2689, 2671, 2647, 2651, 2650, 2669, 2695, 2704, 2706, 2702, 2701, 2700, 2703, 2708, 2711, 2717, 2719, 2720, 2715, 2712, 2680, 2683, 2684, 2685, 2702, 2703, 2686, 2694, 2691, 2695, 2698, 2710, 2716, 2729, 2732, 2742, 2759, 2760, 2742, 2751, 2750, 2780, 2782, 2779, 2778, 2779, 2767, 2770, 2795, 2804, 2806, 2807, 2808, 2809, 2823, 2824, 2825, 2828, 2819, 2823, 2824, 2833, 2807, 2784, 2781, 2793, 2795, 2774, 2737, 2736, 2737, 2742, 2745, 2746, 2749, 2750, 2769, 2774, 2775, 2793, 2796, 2810, 2814, 2808, 2802, 2803, 2806, 2807, 2799, 2798, 2792, 2795, 2798, 2824, 2844, 2831, 2851, 2848, 2852, 2849, 2875, 2871, 2880, 2878, 2882, 2889, 2878, 2883, 2886, 2885, 2886, 2887, 2893, 2894, 2901, 2905, 2907, 2888, 2890, 2895, 2896, 2897, 2874, 2881, 2873, 2874, 2878, 2881, 2893, 2892, 2889, 2890, 2893, 2888, 2883, 2884, 2873, 2871, 2875, 2866, 2839, 2841, 2848, 2845, 2857, 2880, 2881, 2882, 2883, 2888, 2899, 2902, 2905, 2899, 2927, 2953, 2952, 2956, 2963, 2964, 2962, 2968, 2967, 2964, 2966, 2967, 2961, 2960, 2979, 2987, 3007, 3030, 3035, 3042, 3065, 3053, 3054, 3055, 3056, 3059, 3060, 3058, 3069, 3074, 3073, 3065, 3071, 3074, 3087, 3088, 3079, 3086, 3121, 3151, 3147, 3145, 3149, 3150, 3149, 3151, 3152, 3157, 3146, 3153, 3154, 3148, 3146, 3151, 3152, 3157, 3158, 3162, 3167, 3153, 3163, 3169, 3179, 3177, 3181, 3183, 3188, 3187, 3191, 3207, 3214, 3223, 3224, 3225, 3231, 3239, 3241, 3246, 3252, 3253, 3255, 3254, 3251, 3250, 3252, 3251, 3209, 3228, 3233, 3232, 3231, 3239, 3236, 3233, 3236, 3241, 3248, 3247, 3250, 3252, 3284, 3298, 3300, 3301, 3286, 3284, 3290, 3289, 3290, 3291, 3286, 3288, 3295, 3296, 3284, 3283, 3285, 3289, 3251, 3249, 3250, 3251, 3262, 3269, 3270, 3269, 3255, 3258, 3253, 3266, 3270, 3251, 3245, 3247, 3249, 3243, 3244, 3250, 3258, 3287, 3288, 3312, 3320, 3352, 3351, 3369, 3360, 3362, 3364, 3374, 3383, 3364, 3381, 3390, 3391, 3394, 3397, 3407, 3412, 3424, 3432, 3448, 3446, 3447, 3456, 3457, 3467, 3468, 3469, 3476, 3477, 3479, 3514, 3523, 3517, 3518, 3519, 3525, 3528, 3529, 3531, 3532, 3540, 3541, 3544, 3540, 3541, 3542, 3556, 3562, 3553, 3555, 3556, 3558, 3560, 3564, 3551, 3561, 3562, 3561, 3562, 3523, 3516, 3505, 3508, 3511, 3492, 3494, 3512, 3494, 3493, 3487, 3488, 3504, 3522, 3528, 3537, 3550, 3567, 3569, 3582, 3583, 3593, 3613, 3641, 3636, 3647, 3651, 3668, 3679, 3676, 3685, 3665, 3667, 3669, 3674, 3673, 3675, 3689, 3700, 3692, 3698, 3691, 3699, 3685, 3686, 3687, 3696, 3697, 3699, 3700, 3701, 3695, 3696, 3698, 3696, 3699, 3688, 3690, 3699, 3688, 3692, 3693, 3698, 3685, 3680, 3684, 3685, 3686, 3693, 3698, 3705, 3702, 3684, 3686, 3680, 3681, 3683, 3670, 3671, 3676, 3681, 3690, 3681, 3694, 3695, 3708, 3710, 3691, 3687, 3693, 3682, 3678, 3666, 3671, 3675, 3674, 3675, 3672, 3674, 3675, 3674, 3673, 3644, 3646, 3638, 3639, 3637, 3638, 3646, 3652, 3687, 3688, 3693, 3669, 3676, 3683, 3684, 3699, 3690, 3674, 3678, 3677, 3683, 3680, 3703, 3699, 3706, 3707, 3699, 3695, 3705, 3704, 3705, 3719, 3739, 3742, 3743, 3728, 3729, 3730, 3743, 3742, 3752, 3753, 3754, 3760, 3771, 3773, 3782, 3784, 3794, 3814, 3823, 3837, 3840, 3855, 3848, 3861, 3864, 3841, 3844, 3846, 3852, 3853, 3865, 3868, 3877, 3879, 3907, 3932, 3933, 3902, 3914, 3920, 3922, 3908, 3922, 3927, 3926, 3928, 3908, 3912, 3881, 3907, 3890, 3891, 3885, 3900, 3902, 3904, 3901, 3907, 3912, 3914, 3925, 3937, 3938, 3946, 3948, 3947, 3962, 3969, 3974, 3977, 3978, 3985, 3982, 3980, 4004, 4014, 4009, 4010, 4009, 4010, 4029, 4026, 4031, 4032, 4034, 4013, 4016, 4017, 4019, 4020, 4028, 4031, 4032, 4033, 4040, 4041, 4046, 4059, 4065, 4077, 4078, 4079, 4094, 4086, 4089, 4090, 4092, 4091, 4070, 4110, 4111, 4099, 4108, 4112, 4092, 4097, 4099, 4100, 4140, 4141, 4138, 4143, 4142, 4132, 4134, 4143, 4157, 4161, 4185, 4180, 4192, 4193, 4199, 4201, 4203, 4187, 4190, 4187, 4188, 4190, 4200, 4201, 4193, 4192, 4193, 4191, 4194, 4197, 4193, 4200, 4201, 4210, 4216, 4234, 4239, 4225, 4226, 4228, 4232, 4226, 4227, 4235, 4238, 4239, 4244, 4256, 4255, 4254, 4229, 4227, 4233, 4254, 4255, 4251, 4252, 4251, 4247, 4250, 4247, 4249, 4250, 4235, 4236, 4232, 4245, 4247, 4243, 4244, 4246, 4252, 4266, 4271, 4273, 4277, 4279, 4280, 4278, 4279, 4269, 4299, 4300, 4301, 4305, 4290, 4294, 4303, 4304, 4307, 4325, 4326, 4327, 4312, 4313, 4316, 4315, 4319, 4332, 4333, 4356, 4354, 4358, 4366, 4369, 4370, 4381, 4382, 4381, 4402, 4403, 4410, 4409, 4421, 4424, 4448, 4450, 4447, 4475, 4472, 4486, 4464, 4463, 4466, 4465, 4468, 4477, 4478, 4490, 4527, 4528, 4532, 4533, 4525, 4527, 4534, 4556, 4558, 4559, 4555, 4562, 4565, 4566, 4565, 4596, 4604, 4621, 4648, 4649, 4650, 4655, 4656, 4661, 4668, 4682, 4675, 4693, 4686, 4680, 4681, 4685, 4686, 4693, 4694, 4698, 4694, 4706, 4716, 4717, 4718, 4720, 4724, 4730, 4731, 4738, 4740, 4738, 4741, 4740, 4743, 4750, 4751, 4752, 4759, 4796, 4807, 4800, 4815, 4817, 4825, 4853, 4831, 4836, 4839, 4847, 4844, 4851, 4855, 4860, 4858, 4859, 4865, 4874, 4875, 4902, 4910, 4909, 4908, 4922, 4924, 4921, 4922, 4911, 4910, 4907, 4911, 4912, 4936, 4918, 4921, 4915, 4927, 4929, 4931, 4932, 4939, 4944, 4945, 4946, 4949, 4947, 4954, 4964, 4965, 4977, 4978, 4982, 4975, 4978, 4982, 4981, 4985, 4993, 4994, 5000, 5017, 5015, 5039, 5037, 5035, 5047, 5064, 5066, 5072, 5071, 5076, 5078, 5082, 5100, 5102, 5107, 5106, 5104, 5105, 5106, 5105, 5117, 5109, 5110, 5116, 5117, 5118, 5100, 5102, 5103, 5110, 5111, 5126, 5135, 5136, 5134, 5135, 5175, 5177, 5176, 5177, 5203, 5202, 5223, 5226, 5223, 5230, 5256, 5260, 5265, 5267, 5268, 5274, 5273, 5275, 5278, 5284, 5270, 5271, 5272, 5279, 5290, 5298, 5302, 5318, 5321, 5324, 5336, 5335, 5343, 5347, 5352, 5358, 5367, 5368, 5371, 5368, 5373, 5374, 5364, 5367, 5364, 5356, 5360, 5352, 5361, 5356, 5373, 5374, 5375, 5386, 5396, 5398, 5394, 5396, 5398, 5397, 5412, 5413, 5405, 5386, 5389, 5388, 5387, 5403, 5406, 5420, 5429, 5430, 5441, 5449, 5458, 5467, 5466, 5467, 5479, 5478, 5479, 5485, 5525, 5524, 5525, 5524, 5492, 5493, 5497, 5502, 5503, 5504, 5505, 5506, 5515, 5512, 5511, 5513, 5519, 5520, 5521, 5538, 5556, 5567, 5568, 5569, 5587, 5578, 5579, 5581, 5577, 5581, 5591, 5592, 5570, 5572, 5578, 5572, 5575, 5577, 5578, 5586, 5585, 5577, 5578, 5575, 5581, 5569, 5572, 5585, 5576, 5579, 5568, 5569, 5572, 5571, 5572, 5574, 5567, 5572, 5575, 5580, 5583, 5586, 5600, 5602, 5601, 5606, 5609, 5611, 5614, 5623, 5620, 5632, 5640, 5641, 5636, 5620, 5626, 5628, 5636, 5645, 5646, 5623, 5622, 5654, 5673, 5679, 5688, 5695, 5690, 5692, 5709, 5710, 5712, 5722, 5721, 5735, 5746, 5758, 5786, 5766, 5769, 5780, 5781, 5793, 5811, 5812, 5809, 5823, 5825, 5804, 5794, 5795, 5787, 5788, 5819, 5826, 5830, 5832, 5809, 5803, 5804, 5805, 5824, 5825, 5831, 5841, 5842, 5843, 5832, 5838, 5839, 5841, 5840, 5837, 5838, 5832, 5834, 5848, 5852, 5863, 5858, 5859, 5862, 5861, 5863, 5864, 5868, 5875, 5861, 5860, 5837, 5838, 5840, 5841, 5842, 5862, 5877, 5878, 5877, 5878, 5881, ]

// which single measurement is larger than the previous
const answer = xs => xs.filter((depth, i) => depth > xs[i-1]).length
console.log(answer(sweep)) // 1451

// which 3 slot window is lagher than the previous
const windows = xs => xs.map((depth, i) => depth + xs[i-1] + xs[i-2])
console.log(answer(windows(sweep))) // 1395

