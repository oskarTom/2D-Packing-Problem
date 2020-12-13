MODULE SAalgo
  USE mtdefs
  USE mtmod
  USE rectangles
  IMPLICIT NONE
CONTAINS
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
        SELECT CASE (random)
            CASE (1) !MOVE
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
            CASE (2) !ROTATE
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
            CASE (3) !SWAP
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
                
        END SELECT
        
    
        !STEP 5
        df = footp(SAnew) - footp(SA)
        
        !STEP 6
        u = grnd()
        
        !STEP 7
        IF (u < EXP(-df/c)) SA = SAnew
        
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
    PRINT *, "Final footprint:", footp(SA)
  END FUNCTION SA

    FUNCTION MMC(SA)
        IMPLICIT NONE
        TYPE(rect), DIMENSION(SIZE(SA)), INTENT(IN) :: SA
        SAnew = SA
        !STEP 4
        random = igrnd(1,3)
        SELECT CASE (random)
        CASE (1) !MOVE
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
        CASE (2) !ROTATE
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
        CASE (3) !SWAP
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

        END SELECT
    END FUNCTION MMC
  
END MODULE SAalgo
