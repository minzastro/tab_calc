case('ellipse')
    call errorEllipse(temp_values(1), temp_values(2), temp_values(3), temp_values(4), temp_values(5))
    call PrepareRealFormat(5)
    write(*,sFormat) temp_values(1:5)
case('ellipse_gnuplot')
    call errorEllipse(temp_values(1), temp_values(2), temp_values(3), temp_values(4), temp_values(5))
    write(*,*) 'set object 1 ellipse center ', temp_values(1), ',', temp_values(2), &
               'size ', 2*temp_values(4), ',', 2*temp_values(5), &
               'angle ', 180*temp_values(3)/3.1415
