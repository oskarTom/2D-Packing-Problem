MODULE SAalgo
  USE mtdefs
  USE mtmod
  IMPLICIT NONE

  TYPE :: rect
     REAL :: w
     REAL :: h
     REAL :: r(2)
  END type rect
  
CONTAINS

  FUNCTION initialize(n)
    IMPLICIT NONE
    INTEGER :: i=1, j
    INTEGER, INTENT(IN) :: n
    TYPE(rect), DIMENSION(n) :: initialize
    TYPE(rect) :: rectangle
    
    makeRectangle: DO
        rectangle%w = 2*igrnd(1,2)
        rectangle%h = 2*igrnd(1,2)
        rectangle%r(1) = igrnd(-10,10)   !INCREASE THE DIFFERENCE IN THESE igrnd INPUTS 
        rectangle%r(2) = igrnd(-10,10)   !IF YOU INCREASE THE AMOUNT OF RECTANGLES
        
        DO j=1, i-1
            IF (testR(rectangle, initialize(j)) .EQV. .FALSE.) CYCLE makeRectangle
        END DO
        
        initialize(i) = rectangle
        IF (i==n) EXIT
        i = i + 1
     END DO makeRectangle
   END FUNCTION initialize
    
    
    
    !Resturns true if rectangles r1 and r2 don't touch
   LOGICAL FUNCTION testR(r1,r2)
    IMPLICIT NONE
    TYPE(rect), INTENT(IN) :: r1,r2

    !If you want the rectangles to touch,
    !change < or > to <= and >= respectively in all IF statements
    IF (r1%r(1) + r1%w/2 < r2%r(1) - r2%w/2) THEN 
        testR = .TRUE.
    ELSE IF (r1%r(1) - r1%w/2 > r2%r(1) + r2%w/2) THEN
        testR = .TRUE.
    ELSE IF (r1%r(2) + r1%h/2 < r2%r(2) - r2%h/2) THEN
        testR = .TRUE.
    ELSE IF (r1%r(2) - r1%h/2 > r2%r(2) + r2%h/2) THEN 
        testR = .TRUE.
    ELSE
        testR = .FALSE.
    END IF
   END FUNCTION testR
    

   
   !Returns false if inputted rectangles
   !have the same values
   LOGICAL FUNCTION unequal(a,b)
    IMPLICIT NONE
    TYPE(rect), INTENT(IN) :: a,b
    unequal = .TRUE.
    IF(a%w == b%w .AND. a%h == b%h .AND. a%r(1) == b%r(1) &
         .AND. a%r(2) == b%r(2)) unequal = .FALSE.
   END FUNCTION
    
 
    
    
    !Returns the footprint of input array x
   FUNCTION footp(x)
    IMPLICIT NONE
    TYPE(rect), INTENT(IN) :: x(:)
    REAL :: footp
    REAL :: left,right,up,down
    INTEGER :: i
    left = x(1)%r(1) - x(1)%w/2
    right = x(1)%r(1) + x(1)%w/2
    up = x(1)%r(2) + x(1)%h/2
    down = x(1)%r(2) - x(1)%h/2
    
    DO i=2, SIZE(x)
       IF (x(i)%r(1) + x(i)%w/2 > right) THEN
          right = x(i)%r(1) + x(i)%w/2
       ELSE IF(x(i)%r(1) - x(i)%w/2 < left) THEN
          left = x(i)%r(1) - x(i)%w/2
       END IF
       IF (x(i)%r(2) + x(i)%h/2 > up) THEN
          up = x(i)%r(2) + x(i)%h/2
       ELSE IF (x(i)%r(2) - x(i)%h/2 < down) THEN
          down = x(i)%r(2) - x(i)%h/2
       END IF
    END DO
    footp = (right - left)*(up - down)
   END FUNCTION footp
    
    
    !SIMULATED ANNEALING
   FUNCTION SA(x)
    IMPLICIT NONE
    TYPE(rect), DIMENSION(:), INTENT(IN) :: x
    TYPE(rect), DIMENSION(SIZE(x)) :: SA, SAnew
    TYPE(rect) :: newRect, newRect2
    REAL :: df, c=1000, cMin=1, a=0.9999
    REAL(rk_mtmod) :: u
    INTEGER :: i=1, iMax=100, random, rectangle, rectangle2, j, a1=0,a2=0,a3=0
    
    CALL sgrnd(getseed(info=1))
    SA = x
    SAnew = x
    
    PRINT *, "Running SA algorithm..."
    
    MMC: DO
        SAnew = SA
        !STEP 4
        random = igrnd(1,3)
        IF (random == 1) THEN !MOVE
            rectangle = igrnd(1, SIZE(SA))
            newRect%r(1) = SA(rectangle)%r(1)
            newRect%r(2) = SA(rectangle)%r(2)
            newRect%w = SA(rectangle)%w
            newRect%h = SA(rectangle)%h
            random = igrnd(1,4)
            IF (random == 1) THEN 
                newRect%r(1) = newRect%r(1)+1
            ELSE IF (random == 2) THEN 
                newRect%r(2) = newRect%r(2)+1
            ELSE IF (random == 3) THEN 
                newRect%r(1) = newRect%r(1)-1
            ELSE 
                newRect%r(2) = newRect%r(2)-1
            END IF
            
            !Test whether rectangles overlap
            DO j=1, SIZE(SA)
               IF ((testR(newRect, SA(j)) .EQV. .FALSE.) &
                    .AND. unequal(SA(rectangle), SA(j))) CYCLE MMC
            END DO
            a1 = a1+1
            SAnew(rectangle) = newRect
            random = 1
        ELSE IF (random == 2) THEN !ROTATE
            rectangle = igrnd(1, SIZE(SA))
            newRect%r(1) = SA(rectangle)%r(1)
            newRect%r(2) = SA(rectangle)%r(2)
            newRect%w = SA(rectangle)%h
            newRect%h = SA(rectangle)%w
            !Test whether rectangles overlap
            DO j=1, SIZE(SA)
               IF ((testR(newRect, SA(j)) .EQV. .FALSE.) &
                    .AND. unequal(SA(rectangle), SA(j))) CYCLE MMC
            END DO
            SAnew(rectangle) = newRect
            a2 = a2 + 1
        ELSE IF (random == 3) THEN !SWAP
            rectangle = igrnd(1, SIZE(SA))
            newRect2%r(1) = SA(rectangle)%r(1)
            newRect2%r(2) = SA(rectangle)%r(2)
            
            DO
                rectangle2 = igrnd(1, SIZE(SA))
                IF (rectangle2 /= rectangle) EXIT
            END DO
            newRect2%w = SA(rectangle2)%w
            newRect2%h = SA(rectangle2)%h
            
            newRect%r(1) = SA(rectangle2)%r(1)
            newRect%r(2) = SA(rectangle2)%r(2)
            newRect%w = SA(rectangle)%w
            newRect%h = SA(rectangle)%h
            
            !Test whether rectangles overlap
            DO j=1, SIZE(SA)
               IF (((testR(newRect2, SA(j)) .EQV. .FALSE.) &
                    .AND. unequal(SA(rectangle), SA(j))) &
                    .OR. ((testR(newRect, SA(j)) .EQV. .FALSE.) &
                    .AND. unequal(SA(rectangle2), SA(j)) )) THEN
                  CYCLE MMC
                END IF
            END DO
            SAnew(rectangle2) = newRect2
            SAnew(rectangle) = newRect
            a3 = a3+1
            
        END IF
        
    
        !STEP 5
        df = footp(SAnew) - footp(SA)
        
        !STEP 6
        u = grnd()
        
        !STEP 7
        IF (u < EXP(-df/c)) THEN
           
           !   MADE FOR GUI
           !--------------------------------
           PRINT *, random
           PRINT *, "Old:", SA(rectangle)
           PRINT *, "New:", newRect
           IF (random == 3) THEN
              PRINT *, "Old:", SA(rectangle2)
              PRINT *, "New: ", newRect2
           END IF
           !--------------------------------
           SA = SAnew
        END IF
        
        !STEP 8
        i = i+1
        IF (i <= iMax) CYCLE MMC
        
        !STEP 9
        c = a*c
        !STEP 10
        IF (c < cMin) EXIT MMC
        i = 1
    END DO MMC
    PRINT *, "| Moves:", a1, "| Rotations:",a2,"| Swaps:",a3,"|"
    PRINT *, "Initial footprint:", footp(x)
    PRINT *, "Footprint after SA: ", footp(SA)
  END FUNCTION SA

  
END MODULE SAalgo
