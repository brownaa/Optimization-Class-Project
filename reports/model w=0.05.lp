/* Objective function */
min: +830.5975 X_1 +38.05575 X_2 +124.114371334 X_4 +192.72 X_5 +9.5025390625 X_6 +522.1445 X_7 +0.0012550201 X_8
 +156.776628571 X_9 +979.17595 X_10 +913.0125 X_11 +1366.9332 X_12 +7.24835 X_13 +117.61165625 X_14 +14.7545 X_15
 +1672.21125 X_16 +246.41 X_17 +1277.074 X_18 +113.35988 X_19 +416.63 X_20 +137.9045 X_21 +39.7835 X_22
 +1549.144 X_23 +130.15936 X_24 +431.82875 X_25 +754.184 X_26 +723.82176 X_27 +68.4065 X_28 +78.899 X_29
 +66.549 X_30 +4332.232 X_31 +133.01908 X_32 +427.5174 X_33 +2.28576 X_34 +469.624 X_35 +9.5033675 X_36
 +1.52144 X_37 +129.7155 X_38 +18.528095 X_39 +21.43472 X_40 +118.259 X_41 +0.00217294 X_42 +45.712 X_43
 +19.005565 X_44 +49.451 X_45 +48.4985 X_46 +0.008205 X_47 +113.05139 X_48 +3.8035 X_49 +135.9315 X_50
 +57.037 X_51 +59.0666666666 X_52 +11.410875 X_53 +196.715 X_54 +616.5855 X_55 +108.36675 X_56 +79.862 X_57
 +20.9125 X_58 +190.03224 X_59 +319.41125 X_60 +536.034 X_61 +23.7578 X_62 +167.286 X_63 +1.15696875 XG_1
 +1.277453125 XG_7 +0.2019375 XG_9 +0.1818181818 XG_10 +0.20975 XG_12 +0.048 XG_13 +0.2235 XG_15 +0.02863 XG_19
 +0.34975 XG_20 +0.0796 XG_21 +0.09952 XG_23 +0.01556 XG_24 +0.0995 XG_26 +0.04364 XG_27 +0.0265 XG_28
 +0.03736 XG_32 +0.03324 XG_33 +0.00806 XG_36 +0.26536 XG_41 +0.1192 XG_43 +0.010335 XG_44 +0.1235 XG_45
 +0.149 XG_46 +0.199 XG_50 +0.06225 XG_51 +0.11175 XG_56 +0.06225 XG_58 +0.03752 XG_59 +0.0079 XG_62;

/* Constraints */
pork tenderloin: -X_1 -XG_1 +0.333333333334 D_1 = 0;
dry onion soup mix: -X_2 +0.166666666667 D_1 +0.0833333333334 D_3 = 0;
water: -X_3 +0.166666666667 D_1 +0.104166666667 D_3 +0.25 D_11 = 0;
red wine: -X_4 +0.125 D_1 = 0;
garlic: -X_5 +0.03125 D_1 +0.0625 D_4 +0.015625 D_5 +0.025 D_9 +0.00520833333334 D_13 +0.03125 D_18 +0.03125 D_20 = 0;
soy sauce: -X_6 +0.5 D_1 +0.375 D_5 +D_6 +0.25 D_7 = 0;
skinless chicken breast: -X_7 -XG_7 +0.5 D_2 = 0;
salt: -X_8 +0.25 D_2 +0.5 D_4 +0.0625 D_7 +0.125 D_8 +0.03125 D_10 +0.166666666667 D_12 +0.166666666667 D_13
 +0.125 D_15 +0.125 D_16 +0.125 D_18 = 0;
yellow mustard: -X_9 -XG_9 +0.125 D_2 = 0;
honey: -X_10 -XG_10 +0.125 D_2 +0.046875 D_5 = 0;
corn syrup: -X_11 +0.0625 D_2 = 0;
mayonnaise: -X_12 -XG_12 +0.0625 D_2 = 0;
dried onion flakes: -X_13 -XG_13 +0.25 D_2 = 0;
vegetable oil: -X_14 +0.25 D_2 +0.25 D_5 +0.25 D_20 = 0;
mushrooms: -X_15 -XG_15 +0.25 D_2 +0.5 D_6 +D_16 = 0;
colby-jack cheese: -X_16 +0.5 D_2 = 0;
cond. cream of mushroom soup: -X_17 +0.166666666667 D_3 = 0;
pot roast: -X_18 +0.458333333333 D_3 = 0;
olive oil: -X_19 -XG_19 +3 D_4 +0.16666666675 D_5 +D_7 +0.6 D_9 +0.5 D_12 +0.111111111167 D_13 +0.5 D_18 = 0;
salmon filet: -X_20 -XG_20 +D_4 +D_5 = 0;
potatoes: -X_21 -XG_21 +D_8 = 0;
onion: -X_22 +0.25 D_6 +0.25 D_8 +0.0625 D_18 +0.25 D_20 = 0;
butter: -X_23 -XG_23 +0.046875 D_8 +0.0125 D_9 +0.03125 D_10 +0.0416666666667 D_11 +0.09375 D_15 +0.125 D_16 = 0;
milk: -X_24 -XG_24 +0.5 D_8 +0.25 D_16 = 0;
cheddar cheese: -X_25 +0.375 D_8 = 0;
cream cheese: -X_26 -XG_26 +0.09375 D_14 = 0;
sugar: -X_27 -XG_27 +0.15625 D_10 +0.0104166666667 D_11 +0.0625 D_14 +0.15625 D_15 +0.3125 D_16 +0.3125 D_17 = 0;
egg: -X_28 -XG_28 +0.25 D_10 +0.25 D_14 +0.375 D_15 = 0;
pumpkin puree: -X_29 +0.0625 D_14 = 0;
green beans(can): -X_30 +0.4 D_9 = 0;
parmesan cheese: -X_31 +0.05 D_9 = 0;
pineapple juice: -X_32 -XG_32 +0.09375 D_15 = 0;
flour: -X_33 -XG_33 +0.0234375 D_10 +0.1875 D_15 +0.3125 D_16 +0.125 D_17 = 0;
baking powder: -X_34 +0.25 D_15 +1.5 D_16 = 0;
pineapple: -X_35 +0.125 D_15 = 0;
balsamic vinegar: -X_36 -XG_36 +0.75 D_5 +0.25 D_7 = 0;
ginger root: -X_37 +0.25 D_5 = 0;
sweet potato: -X_38 +0.625 D_10 = 0;
heavy cream: -X_39 +0.25 D_10 = 0;
pecans: -X_40 +0.0625 D_10 = 0;
green beans(whole): -X_41 -XG_41 +0.25 D_11 = 0;
garlic salt: -X_42 +0.0833333333334 D_11 = 0;
cherry tomato: -X_43 -XG_43 +0.333333333334 D_11 +D_18 +0.5 D_20 = 0;
tofu: -X_44 -XG_44 +4 D_6 = 0;
zucchini: -X_45 -XG_45 +0.5 D_6 +0.375 D_7 = 0;
red bell pepper: -X_46 -XG_46 +0.5 D_6 +0.25 D_7 +0.125 D_20 = 0;
sriracha: -X_47 +0.0625 D_6 = 0;
sesame oil: -X_48 +D_6 = 0;
jalapeno pepper: -X_49 +0.5 D_6 = 0;
eggplant: -X_50 -XG_50 +0.25 D_7 = 0;
peach(sliced): -X_51 -XG_51 +D_16 +0.75 D_17 = 0;
blackberry: -X_52 +0.25 D_16 = 0;
Brussels sprouts: -X_53 +4 D_12 = 0;
broccoli(head): -X_54 +0.333333333334 D_13 = 0;
9in pie crust: -X_55 +D_17 = 0;
strawberry: -X_56 -XG_56 +0.5 D_17 = 0;
blueberry: -X_57 +0.5 D_17 = 0;
parsley: -X_58 -XG_58 +0.0625 D_18 = 0;
penne pasta: -X_59 -XG_59 +D_18 = 0;
mozzarella cheese: -X_60 +0.25 D_18 = 0;
lean ground beef: -X_61 +0.25 D_20 = 0;
carrot: -X_62 -XG_62 +0.5 D_20 = 0;
couscous: -X_63 +0.5 D_19 = 0;
Pork Tenderloin: -D_1 +D_1,veg +D_1,!veg = 0;
Aussie Chicken: -D_2 +D_2,veg +D_2,!veg = 0;
Awesome Slow Cooker Pot Roast: -D_3 +D_3,veg +D_3,!veg = 0;
Baked Salmon II: -D_4 +D_4,veg +D_4,!veg = 0;
Ginger Glazed Mahi Mahi: -D_5 +D_5,veg +D_5,!veg = 0;
Grilled Tofu Skewers with Sriracha Sauce: -D_6 +D_6,veg +D_6,!veg = 0;
Grilled Vegetables with Balsamic Vinegar: -D_7 +D_7,veg +D_7,!veg = 0;
Creamy Au Gratin Potatoes: -D_8 +D_8,veg +D_8,!veg = 0;
Garlic Green Beans: -D_9 +D_9,veg +D_9,!veg = 0;
Gourmet Sweet Potato Classic: -D_10 +D_10,veg +D_10,!veg = 0;
Green Beans with Cherry Tomatoes: -D_11 +D_11,veg +D_11,!veg = 0;
Roasted Brussels Sprouts: -D_12 +D_12,veg +D_12,!veg = 0;
Roasted Garlic Lemon Broccoli: -D_13 +D_13,veg +D_13,!veg = 0;
Double Layer Pumpkin Cheesecake: -D_14 +D_14,veg +D_14,!veg = 0;
Fresh Pineapple Upside Down Cake: -D_15 +D_15,veg +D_15,!veg = 0;
Peach and Blackberry Cobbler: -D_16 +D_16,veg +D_16,!veg = 0;
Summer Fruit Galettes: -D_17 +D_17,veg +D_17,!veg = 0;
Pasta with Tomato and Bocconcini: -D_18 +D_18,veg +D_18,!veg = 0;
Couscous: -D_19 +D_19,veg +D_19,!veg = 0;
Chunky Spiced Ground Beef Skillet Dinner: -D_20 +D_20,veg +D_20,!veg = 0;
Veg Main: +D_6,veg +D_7,veg +D_18,veg = 4;
Veg Side LB: +D_8,veg +D_9,veg +D_10,veg +D_11,veg +D_12,veg +D_13,veg +D_19,veg >= 8;
Veg Side UB: +D_8,veg +D_9,veg +D_10,veg +D_11,veg +D_12,veg +D_13,veg +D_19,veg <= 12;
Veg Dessert: +D_14,veg +D_15,veg +D_16,veg +D_17,veg = 4;
Nonveg Main: +D_1,!veg +D_2,!veg +D_3,!veg +D_4,!veg +D_5,!veg +D_20,!veg = 6;
Nonveg Side LB: +D_8,!veg +D_9,!veg +D_10,!veg +D_11,!veg +D_12,!veg +D_13,!veg +D_19,!veg >= 12;
Nonveg Side UB: +D_8,!veg +D_9,!veg +D_10,!veg +D_11,!veg +D_12,!veg +D_13,!veg +D_19,!veg <= 18;
Nonveg Dessert: +D_14,!veg +D_15,!veg +D_16,!veg +D_17,!veg = 6;
Veg CAL Lower Limit: : +325.247916667 D_1,veg +1490.6875 D_2,veg +662.55 D_3,veg +808.9625 D_4,veg +552.358333344 D_5,veg +280.65 D_6,veg
 +192.3 D_7,veg +470.884375 D_8,veg +353.0275 D_9,veg +290.13125 D_10,veg +122.978125 D_11,veg +107.65 D_12,veg
 +83.3107638956 D_13,veg +145.225 D_14,veg +458.790625 D_15,veg +711.29375 D_16,veg +1087.34375 D_17,veg
 +401.96875 D_18,veg +88 D_19,veg +231.60625 D_20,veg >= 3400;
Veg Fiber (g) Lower Limit: : +0.423958333333 D_1,veg +1.3875 D_2,veg +0.166666666667 D_3,veg +0.18125 D_4,veg +0.078125 D_5,veg +4.4 D_6,veg
 +5.75 D_7,veg +2.8 D_8,veg +2.4725 D_9,veg +3.2296875 D_10,veg +4.16666666666 D_11,veg +4 D_12,veg +5.3484375 D_13,veg
 +0.44375 D_14,veg +2.234375 D_15,veg +5.8625 D_16,veg +8.425 D_17,veg +7.750625 D_18,veg +D_19,veg +3.565625 D_20,veg >= 50;
Veg Protein(g) Lower Limit: : +45.2270833333 D_1,veg +109.4625 D_2,veg +59.4833333333 D_3,veg +61.7375 D_4,veg +61.75625 D_5,veg +13.425 D_6,veg
 +3.75 D_7,veg +18.9625 D_8,veg +3.515 D_9,veg +4.02109375 D_10,veg +1.91666666667 D_11,veg +4 D_12,veg
 +5.71145833334 D_13,veg +3.35625 D_14,veg +5.49375 D_15,veg +10.35625 D_16,veg +9.8625 D_17,veg +23.875 D_18,veg
 +3 D_19,veg +8.04375 D_20,veg >= 100;
Veg VA Lower Limit: : +0.00166666666667 D_1,veg +0.50375 D_2,veg +0.003 D_3,veg +0.025 D_4,veg +0.025 D_5,veg +0.59 D_6,veg
 +0.32875 D_7,veg +0.188828125 D_8,veg +0.0931875 D_9,veg +3.31403125 D_10,veg +0.311041666667 D_11,veg
 +0.16 D_12,veg +0.253333333333 D_13,veg +0.53128125 D_14,veg +0.16278125 D_15,veg +0.279875 D_16,veg
 +0.09 D_17,veg +4.293125 D_18,veg +102.36875 D_20,veg >= 2;
Veg VC Lower Limit: : +0.0327604166667 D_1,veg +0.075375 D_2,veg +0.0441875 D_4,veg +0.012859375 D_5,veg +2.29825 D_6,veg +1.2575 D_7,veg
 +0.36625 D_8,veg +0.065675 D_9,veg +0.309375 D_10,veg +0.275583333334 D_11,veg +1.6 D_12,veg +3.017015625 D_13,veg
 +0.01075 D_14,veg +0.3653125 D_15,veg +0.325 D_16,veg +1.9975 D_17,veg +0.75253125 D_18,veg +3.80759375 D_20,veg >= 2;
Veg Ca Lower Limit: : +0.0806875 D_1,veg +1.657375 D_2,veg +0.336166666666 D_3,veg +0.062875 D_4,veg +0.05178125 D_5,veg +0.2765 D_6,veg
 +0.0313125 D_7,veg +0.46325 D_8,veg +0.09955 D_9,veg +0.0342890625 D_10,veg +0.0435 D_11,veg +0.0401666666667 D_12,veg
 +0.0981145833334 D_13,veg +0.026625 D_14,veg +0.09621875 D_15,veg +0.5103125 D_16,veg +0.074875 D_17,veg
 +14.7591875 D_18,veg +0.5 D_19,veg +1.2644375 D_20,veg >= 2;
Veg Fe Lower Limit: : +0.132 D_1,veg +0.1398125 D_2,veg +0.317333333334 D_3,veg +0.1955 D_4,veg +0.184453125 D_5,veg +0.18825 D_6,veg
 +0.0555625 D_7,veg +0.249875 D_8,veg +0.00765 D_9,veg +0.055140625 D_10,veg +0.0415 D_11,veg +0.0816666666666 D_12,veg
 +0.0845000000001 D_13,veg +0.03634375 D_14,veg +0.112125 D_15,veg +0.29525 D_16,veg +0.13525 D_17,veg
 +23.1689375 D_18,veg +1.5 D_19,veg +4.01225 D_20,veg >= 2;
Veg CAL Upper Limit: : +325.247916667 D_1,veg +1490.6875 D_2,veg +662.55 D_3,veg +808.9625 D_4,veg +552.358333344 D_5,veg +280.65 D_6,veg
 +192.3 D_7,veg +470.884375 D_8,veg +353.0275 D_9,veg +290.13125 D_10,veg +122.978125 D_11,veg +107.65 D_12,veg
 +83.3107638956 D_13,veg +145.225 D_14,veg +458.790625 D_15,veg +711.29375 D_16,veg +1087.34375 D_17,veg
 +401.96875 D_18,veg +88 D_19,veg +231.60625 D_20,veg <= 4600;
Veg Fat (g) Upper Limit: : +12.4552083333 D_1,veg +92.5875 D_2,veg +43.3333333333 D_3,veg +60.14375 D_4,veg +25.3609375011 D_5,veg
 +18.65 D_6,veg +14.125 D_7,veg +25.378125 D_8,veg +11.915 D_9,veg +12.996875 D_10,veg +7.65833333334 D_11,veg
 +6.75 D_12,veg +2.17031250075 D_13,veg +8.7625 D_14,veg +19.85 D_15,veg +25.025 D_16,veg +41.65 D_17,veg
 +15.134375 D_18,veg +4.821875 D_20,veg <= 195;
Veg Na (mg) Upper Limit: : +575.755208333 D_1,veg +2589.96875 D_2,veg +568.883333333 D_3,veg +1298.29375 D_4,veg +354.5484375 D_5,veg
 +640.225000001 D_6,veg +306.34375 D_7,veg +696.16875 D_8,veg +542.0225 D_9,veg +143.45546875 D_10,veg
 +220.916666667 D_11,veg +415.583333334 D_12,veg +454.703645833 D_13,veg +82.8875 D_14,veg +563.803125 D_15,veg
 +1225.61875 D_16,veg +820.8125 D_17,veg +469.240625 D_18,veg +4 D_19,veg +37.796875 D_20,veg <= 7200;
Veg carb (g) Upper Limit: : +2.05208333333 D_1,veg +62.675 D_2,veg +4.08333333334 D_3,veg +0.0875 D_4,veg +15.0890625 D_5,veg +15.4 D_6,veg
 +15.625 D_7,veg +43.275 D_8,veg +5.02 D_9,veg +54.5171875 D_10,veg +11.875 D_11,veg +12 D_12,veg +13.340625 D_13,veg
 +14.71875 D_14,veg +68.4593750001 D_15,veg +118.6625 D_16,veg +172.675 D_17,veg +100.5875 D_18,veg +18 D_19,veg
 +12.01875 D_20,veg <= 900;
Nonveg CAL Lower Limit: : +325.247916667 D_1,!veg +1490.6875 D_2,!veg +662.55 D_3,!veg +808.9625 D_4,!veg +552.358333344 D_5,!veg
 +280.65 D_6,!veg +192.3 D_7,!veg +470.884375 D_8,!veg +353.0275 D_9,!veg +290.13125 D_10,!veg +122.978125 D_11,!veg
 +107.65 D_12,!veg +83.3107638956 D_13,!veg +145.225 D_14,!veg +458.790625 D_15,!veg +711.29375 D_16,!veg
 +1087.34375 D_17,!veg +401.96875 D_18,!veg +88 D_19,!veg +231.60625 D_20,!veg >= 5100;
Nonveg Fiber (g) Lower Limit: : +0.423958333333 D_1,!veg +1.3875 D_2,!veg +0.166666666667 D_3,!veg +0.18125 D_4,!veg +0.078125 D_5,!veg
 +4.4 D_6,!veg +5.75 D_7,!veg +2.8 D_8,!veg +2.4725 D_9,!veg +3.2296875 D_10,!veg +4.16666666666 D_11,!veg
 +4 D_12,!veg +5.3484375 D_13,!veg +0.44375 D_14,!veg +2.234375 D_15,!veg +5.8625 D_16,!veg +8.425 D_17,!veg
 +7.750625 D_18,!veg +D_19,!veg +3.565625 D_20,!veg >= 75;
Nonveg Protein(g) Lower Limit: : +45.2270833333 D_1,!veg +109.4625 D_2,!veg +59.4833333333 D_3,!veg +61.7375 D_4,!veg +61.75625 D_5,!veg
 +13.425 D_6,!veg +3.75 D_7,!veg +18.9625 D_8,!veg +3.515 D_9,!veg +4.02109375 D_10,!veg +1.91666666667 D_11,!veg
 +4 D_12,!veg +5.71145833334 D_13,!veg +3.35625 D_14,!veg +5.49375 D_15,!veg +10.35625 D_16,!veg +9.8625 D_17,!veg
 +23.875 D_18,!veg +3 D_19,!veg +8.04375 D_20,!veg >= 150;
Nonveg VA Lower Limit: : +0.00166666666667 D_1,!veg +0.50375 D_2,!veg +0.003 D_3,!veg +0.025 D_4,!veg +0.025 D_5,!veg +0.59 D_6,!veg
 +0.32875 D_7,!veg +0.188828125 D_8,!veg +0.0931875 D_9,!veg +3.31403125 D_10,!veg +0.311041666667 D_11,!veg
 +0.16 D_12,!veg +0.253333333333 D_13,!veg +0.53128125 D_14,!veg +0.16278125 D_15,!veg +0.279875 D_16,!veg
 +0.09 D_17,!veg +4.293125 D_18,!veg +102.36875 D_20,!veg >= 3;
Nonveg VC Lower Limit: : +0.0327604166667 D_1,!veg +0.075375 D_2,!veg +0.0441875 D_4,!veg +0.012859375 D_5,!veg +2.29825 D_6,!veg
 +1.2575 D_7,!veg +0.36625 D_8,!veg +0.065675 D_9,!veg +0.309375 D_10,!veg +0.275583333334 D_11,!veg
 +1.6 D_12,!veg +3.017015625 D_13,!veg +0.01075 D_14,!veg +0.3653125 D_15,!veg +0.325 D_16,!veg +1.9975 D_17,!veg
 +0.75253125 D_18,!veg +3.80759375 D_20,!veg >= 3;
Nonveg Ca Lower Limit: : +0.0806875 D_1,!veg +1.657375 D_2,!veg +0.336166666666 D_3,!veg +0.062875 D_4,!veg +0.05178125 D_5,!veg
 +0.2765 D_6,!veg +0.0313125 D_7,!veg +0.46325 D_8,!veg +0.09955 D_9,!veg +0.0342890625 D_10,!veg +0.0435 D_11,!veg
 +0.0401666666667 D_12,!veg +0.0981145833334 D_13,!veg +0.026625 D_14,!veg +0.09621875 D_15,!veg +0.5103125 D_16,!veg
 +0.074875 D_17,!veg +14.7591875 D_18,!veg +0.5 D_19,!veg +1.2644375 D_20,!veg >= 3;
Nonveg Fe Lower Limit: : +0.132 D_1,!veg +0.1398125 D_2,!veg +0.317333333334 D_3,!veg +0.1955 D_4,!veg +0.184453125 D_5,!veg +0.18825 D_6,!veg
 +0.0555625 D_7,!veg +0.249875 D_8,!veg +0.00765 D_9,!veg +0.055140625 D_10,!veg +0.0415 D_11,!veg +0.0816666666666 D_12,!veg
 +0.0845000000001 D_13,!veg +0.03634375 D_14,!veg +0.112125 D_15,!veg +0.29525 D_16,!veg +0.13525 D_17,!veg
 +23.1689375 D_18,!veg +1.5 D_19,!veg +4.01225 D_20,!veg >= 3;
Nonveg CAL Upper Limit: : +325.247916667 D_1,!veg +1490.6875 D_2,!veg +662.55 D_3,!veg +808.9625 D_4,!veg +552.358333344 D_5,!veg
 +280.65 D_6,!veg +192.3 D_7,!veg +470.884375 D_8,!veg +353.0275 D_9,!veg +290.13125 D_10,!veg +122.978125 D_11,!veg
 +107.65 D_12,!veg +83.3107638956 D_13,!veg +145.225 D_14,!veg +458.790625 D_15,!veg +711.29375 D_16,!veg
 +1087.34375 D_17,!veg +401.96875 D_18,!veg +88 D_19,!veg +231.60625 D_20,!veg <= 6900;
Nonveg Fat (g) Upper Limit: : +12.4552083333 D_1,!veg +92.5875 D_2,!veg +43.3333333333 D_3,!veg +60.14375 D_4,!veg +25.3609375011 D_5,!veg
 +18.65 D_6,!veg +14.125 D_7,!veg +25.378125 D_8,!veg +11.915 D_9,!veg +12.996875 D_10,!veg +7.65833333334 D_11,!veg
 +6.75 D_12,!veg +2.17031250075 D_13,!veg +8.7625 D_14,!veg +19.85 D_15,!veg +25.025 D_16,!veg +41.65 D_17,!veg
 +15.134375 D_18,!veg +4.821875 D_20,!veg <= 292.5;
Nonveg Na (mg) Upper Limit: : +575.755208333 D_1,!veg +2589.96875 D_2,!veg +568.883333333 D_3,!veg +1298.29375 D_4,!veg +354.5484375 D_5,!veg
 +640.225000001 D_6,!veg +306.34375 D_7,!veg +696.16875 D_8,!veg +542.0225 D_9,!veg +143.45546875 D_10,!veg
 +220.916666667 D_11,!veg +415.583333334 D_12,!veg +454.703645833 D_13,!veg +82.8875 D_14,!veg +563.803125 D_15,!veg
 +1225.61875 D_16,!veg +820.8125 D_17,!veg +469.240625 D_18,!veg +4 D_19,!veg +37.796875 D_20,!veg <= 10800;
Nonveg carb (g) Upper Limit: : +2.05208333333 D_1,!veg +62.675 D_2,!veg +4.08333333334 D_3,!veg +0.0875 D_4,!veg +15.0890625 D_5,!veg
 +15.4 D_6,!veg +15.625 D_7,!veg +43.275 D_8,!veg +5.02 D_9,!veg +54.5171875 D_10,!veg +11.875 D_11,!veg
 +12 D_12,!veg +13.340625 D_13,!veg +14.71875 D_14,!veg +68.4593750001 D_15,!veg +118.6625 D_16,!veg
 +172.675 D_17,!veg +100.5875 D_18,!veg +18 D_19,!veg +12.01875 D_20,!veg <= 1350;
