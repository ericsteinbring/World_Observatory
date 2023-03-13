function zero, image
; Eric Steinbring, 11 April 2005

s = size(image)
if s(0) ne 2 then print, 'ZERO requires a two dimensional image'

for i = 0, s(1)-1 do begin
 for j = 0, s(2)-1 do begin
  if (image(i,j) lt 0.) then begin
   image(i,j) = 0.
  endif
 endfor
endfor

return, image

end
