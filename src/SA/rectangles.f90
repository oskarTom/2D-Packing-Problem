MODULE Rectangles
    USE mtdefs
    USE mtmod
    IMPLICIT NONE
  
    TYPE :: rect
       REAL :: w
       REAL :: h
       REAL :: r(2)
    END type rect
    
    CONTAINS

    !Initializes an array of rectangles
    !with random dimentions and coordinates
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
END MODULE