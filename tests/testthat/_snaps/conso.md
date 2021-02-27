# conso_output

    Code
      c
    Output
      $vcons
         vcons times back12
      1      0   0.0    200
      2   2400  10.0    150
      3   1800  20.0    100
      4   1200  40.0      0
      5      0  40.0     NA
      6      0  40.0     NA
      7      0  40.0     NA
      8      0  41.7     NA
      9      0  41.7     NA
      10     0  44.7     NA
      11     0  44.7     NA
      12     0  45.2     NA
      
      $rules
             rule1  name1 temps1 rule2   name2 temps2 empty nameE tempsE
      back12   150 retour     10   100 reserve     20     0    AF     40
      
      $dtcurve
           times depths pressure
      1      0.0      0      1.0
      2      0.0     20      3.0
      21    10.0     20      3.0
      211   20.0     20      3.0
      2111  40.0     20      3.0
      3     40.0     20      3.0
      4     41.7      3      1.3
      5     44.7      3      1.3
      6     45.2      0      1.0
      
      $hour
      [1]  0.0 45.2
      
      attr(,"class")
      [1] "conso"

---

    Code
      conso(dive, back15)
    Output
      $vcons
         vcons times back15
      1    0.0   0.0 200.00
      2 2400.0  12.5 150.00
      3 1650.0  25.0 100.00
      4  900.0  40.0  40.00
      5   73.1  41.7  35.13
      6   78.0  44.7  29.93
      7   11.5  45.2  29.16
      
      $rules
             rule1  name1 temps1 rule2   name2 temps2 empty nameE tempsE
      back15   150 retour   12.5   100 reserve     25     0    AF     NA
      
      $dtcurve
          times depths pressure
      1     0.0      0      1.0
      2     0.0     20      3.0
      21   12.5     20      3.0
      211  25.0     20      3.0
      3    40.0     20      3.0
      4    41.7      3      1.3
      5    44.7      3      1.3
      6    45.2      0      1.0
      
      $hour
      [1]  0.0 45.2
      
      attr(,"class")
      [1] "conso"

---

    Code
      conso(dive, list(relay, back))
    Output
      $vcons
         vcons times relay12 back12
      1    0.0   0.0  200.00    200
      2 2400.0  16.0  120.00    200
      3 1440.0  26.0  120.00    150
      4  840.0  36.0  120.00    100
      5  240.0  40.0  100.00    100
      6   73.1  41.7   93.91    100
      7   78.0  44.7   87.41    100
      8   11.5  45.2   86.45    100
      
      $rules
              rule1  name1 temps1 rule2   name2 temps2 Empty nameE tempsE
      relay12   120 retour     16   120 reserve     16     0    AF     NA
      back12    150 retour     26   100 reserve     36     0    AF     NA
      
      $dtcurve
           times depths pressure
      1      0.0      0      1.0
      2      0.0     20      3.0
      21    16.0     20      3.0
      211   26.0     20      3.0
      2111  36.0     20      3.0
      3     40.0     20      3.0
      4     41.7      3      1.3
      5     44.7      3      1.3
      6     45.2      0      1.0
      
      $hour
      [1]  0.0 45.2
      
      attr(,"class")
      [1] "conso"

---

    Code
      conso(dive, back)
    Warning <simpleWarning>
      No tank is available between 40 and 40 minutes so you died. Try again !
    Warning <simpleWarning>
      No tank is available between 40 and 41.7 minutes so you died. Try again !
    Warning <simpleWarning>
      No tank is available between 41.7 and 44.7 minutes so you died. Try again !
    Warning <simpleWarning>
      No tank is available between 44.7 and 45.2 minutes so you died. Try again !
    Output
      $vcons
         vcons times back12
      1      0   0.0    200
      2   2400  10.0    150
      3   1800  20.0    100
      4   1200  40.0      0
      5      0  40.0     NA
      6      0  40.0     NA
      7      0  40.0     NA
      8      0  41.7     NA
      9      0  41.7     NA
      10     0  44.7     NA
      11     0  44.7     NA
      12     0  45.2     NA
      
      $rules
             rule1  name1 temps1 rule2   name2 temps2 empty nameE tempsE
      back12   150 retour     10   100 reserve     20     0    AF     40
      
      $dtcurve
           times depths pressure
      1      0.0      0      1.0
      2      0.0     20      3.0
      21    10.0     20      3.0
      211   20.0     20      3.0
      2111  40.0     20      3.0
      3     40.0     20      3.0
      4     41.7      3      1.3
      5     44.7      3      1.3
      6     45.2      0      1.0
      
      $hour
      [1]  67.0 112.2
      
      attr(,"class")
      [1] "conso"

---

    Code
      conso(dive, back15)
    Output
      $vcons
         vcons times back15
      1    0.0   0.0 200.00
      2 2400.0  12.5 150.00
      3 1650.0  25.0 100.00
      4  900.0  40.0  40.00
      5   73.1  41.7  35.13
      6   78.0  44.7  29.93
      7   11.5  45.2  29.16
      
      $rules
             rule1  name1 temps1 rule2   name2 temps2 empty nameE tempsE
      back15   150 retour   12.5   100 reserve     25     0    AF     NA
      
      $dtcurve
          times depths pressure
      1     0.0      0      1.0
      2     0.0     20      3.0
      21   12.5     20      3.0
      211  25.0     20      3.0
      3    40.0     20      3.0
      4    41.7      3      1.3
      5    44.7      3      1.3
      6    45.2      0      1.0
      
      $hour
      [1]  67.0 112.2
      
      attr(,"class")
      [1] "conso"

---

    Code
      conso(dive, list(relay, back))
    Output
      $vcons
         vcons times relay12 back12
      1    0.0   0.0  200.00    200
      2 2400.0  16.0  120.00    200
      3 1440.0  26.0  120.00    150
      4  840.0  36.0  120.00    100
      5  240.0  40.0  100.00    100
      6   73.1  41.7   93.91    100
      7   78.0  44.7   87.41    100
      8   11.5  45.2   86.45    100
      
      $rules
              rule1  name1 temps1 rule2   name2 temps2 Empty nameE tempsE
      relay12   120 retour     16   120 reserve     16     0    AF     NA
      back12    150 retour     26   100 reserve     36     0    AF     NA
      
      $dtcurve
           times depths pressure
      1      0.0      0      1.0
      2      0.0     20      3.0
      21    16.0     20      3.0
      211   26.0     20      3.0
      2111  36.0     20      3.0
      3     40.0     20      3.0
      4     41.7      3      1.3
      5     44.7      3      1.3
      6     45.2      0      1.0
      
      $hour
      [1]  67.0 112.2
      
      attr(,"class")
      [1] "conso"

