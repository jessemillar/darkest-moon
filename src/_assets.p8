pico-8 cartridge // http://www.pico-8.com
version 8
__lua__

__gfx__
0000000000000000ccccccccccccc16666ccccccccccc55555ccccccccccc55555cccccccccc999999cccccccccccccccccccccccccccccccccccccccccccccc
1110000011000000cccccccccc9666666666ccccccc555555555ccccccc555555555cccccc9999090099ccccccccccccccc555cccc1661ccccc555ccccc777cc
2211000021100000ccccccccc166666666666cccccc5555555555cccccc5555555555ccccc7999666666ccccccc888cccc55757cc161111ccc55757ccc77777c
3331100033110000ccccccccc111166675557ccccc55555775557ccccc55555775557ccccc99966666666ccccc88888ccc57090ccc17090ccc57090ccc77090c
4221100044221000cccccccc1111117777777ccccc55557777777ccccc55557777777ccccc96667777777cccccc8090ccc16777ccc57777ccc22222ccc77777c
5511100055110000cccccccc1111157707990ccccc55557707990ccccc55557707990cccccc6677a07990cccccc5777ccc61777ccc57777cc222222cccc777cc
66d5100066dd51008ccccccc1111557707990ccccc55557707990ccccc55557707990cccccc6777a07990cccccc5777ccc17777ccc57777cc227777ccc7777cc
776d100077776d51888ccccccc15557777977cccccc2257777977cccccc5557777977ccccccc777777977ccccccccccccccccccccccccccccccccccccccccccc
88221000888421008888ccccccc5557777777ccccc222222277722ccccc5557777777cccccccc7777777ccccccc999ccccccccccccccccccccc7755ccccccccc
94221000999421007557cccccc55557777777ccccc222222222222ccccc66d77777776ccccccc7777777cccccc9fff9cccc999ccccccccccc7777667ccc7c7cc
a9421000aa9942100790cccccc55557777777cccc225222222222cccccd656d777777dccccccc77777777cccc9f44f4ccc9fff9cccccccccc7777666ccc67ccc
bb331000bbb331007777cccccc55157777777cccc225557772227cccccd55667777776cccccc777777777cccc9ff4f4cc9f44f4ccccccccc77775577cccc7ccc
ccd51000ccdd51008888ccccc222211777777cccc255557777777ccccc6556d777777ccccccc777777777ccc9ffffff9c9ff4f4ccccccccc77675577cccc7ccc
dd511000dd5110008888ccccc2332117777777ccc2555577777777ccccd55d77777777ccccc77777777777cc9f7777f99f7777f9cccccccc66677777cccc7ccc
ee421000ee4442107777ccccc2332117777777cccc555577777777cccc555577777777ccccc77777777777cc99ffff9999ffff99cccfcccc77767777cccc76cc
f9421000fff942107779ccccc2222119777799ccccc55999777799ccccc55999777799cccccc7999777799ccc999999cc999999cccc9fcccc777777cccc7d7cc
ccc24ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc99ccc99ccccccccccccccc7ccccccccccccccccccccccccccccccc
ccc544cccccccccccccccccccccc966666ccccccccccc55555ccccccccccc55555cccccccc79999999cccccccccccc67777ccccccccccccccccccccccccccccc
ccc542ccccccccccccccccccc11666666666ccccccc555555555ccccccc555555555cccccc9999090099ccccccccc7677777cccccccccccc7ccccccccccccccc
ccc24ccccccccccccccccccc1111666666666cccccc5555555555cccccc5555555555ccccc9999666666cccccccc77667777cccccccccc67777cccccc7cccc67
ccc54ccccccccccccccccccc1111166675557ccccc55555775557ccccc55555775557ccccc99966666666ccccccc767777777cccccccc7677777cccccc77777d
ccc44ccccccccccccccccccc1111117777777ccccc55557777777ccccc55557777777cccccc6667777777ccccccc777557665ccccccc77667777ccccc76cccc7
cc244cccccccccccccccccccc111157707990ccccc55557707990ccccc55557707990cccccc6677a07990cccccccc77557665ccccccc767777777ccccccccccc
cc2445ccccccc8888ccccccccc11557707990ccccc55557707990ccccc55557707990cccccc6777a07990cccccccc7777767cccccccc777557665ccccccccccc
ccc452cccccc8888888ccccccc15557777977cccccc2257777977cccccc5557777977ccccccc777777977ccccccccc6cc77cccccccccc77557665ccccccccccc
ccc45cccccccc8888888ccccccc5557777777ccccc222222277722ccccc5557777777cccccccc7777777ccccccc6777776ccccccccccc7777767cccccccccccc
ccc44cccccccc8887557cccccc55557777777cccc2222222222222ccccc66d77777776cccccc777777777cccccc76c676dccccccccccc777777ccccccccccccc
cccc4cccccccc8570790cccccc55157777777cccc225222222222cccccd556d777777dccccc77777777777ccccc7c7777dccccccccc676676dcccccccccccccc
cccc4cccccccc5577777ccccc222211777777ccc2255557772227cccccd55667777776cccc77777777777ccccccc7cc6ccdcccccccc767777dcccccccccccccc
cccc42cccccc88888888ccccc2332117777777cc2c555577777777cccc6556d7777777ccccc77777777777cccccccc777cccccccccc7cc7677cccccccccccc76
cccc4cccccc888888888ccccc2332117777777cccc555577777777ccccd55d77777777ccccc77777777777ccccccc7c7c7ccccccccc7c7c7c7ccccccc7777777
cccc2cccccccc5997779ccccc2222119777799ccccc55999777799ccccc55999777799cccccc7999777799ccccccc77cc77cccccccccc77cc77cccccc67dcccc
ccccccccccccccccccccccccccccccccccccccccccccc55555ccccccccccc55555ccccccccc999999ccccccccccccccc7ccccccccccccccccccccccc00000000
ccccccccccccccccccccccccc11966666cccccccccc555555555ccccccc555555555ccccc9999090099cccccc7cccc67777cccccccccccccc7cccccc00000000
cccccccccccccccccccccccc11666666666cccccccc5555555555cccccc5555555555cccc7999666666ccccc67ccc7677777ccccccccccc67777cccc00000000
cc333ccccccccccccccccccc111666666666cccccc55555775557ccccc55555775557cccc99966666666ccccc7cc77667777cccccccccc7677777ccc00000000
cc333ccccccccccccccccccc111166675557cccccc55557777777ccccc55557777777cccc96667777777ccccc77c767777777cccccccc77667777ccc00000000
cc3b3ccccccccccccccccccc111117777777cccccc55557707990ccccc55557707990ccccc6677007990cccccc7c777557665cccccccc767777777cc00000000
cc333ccccccccc888cccccccc11157007990cccccc55557707990ccccc55557707990ccccc6777a07990cccccc76c77557665cccccccc777557665cc00000000
cc333cccccccc888888cccccc11557777997ccccccc2257777977cccccc5557777977555ccc777777977ccccc77dc7777767cccccccccc77557665cc00000000
cccccccccccc38888888cccccc5557777977ccccccc22222277722cccccc55577777755ccccc7777777ccccccc776c6cc77ccccccccccc7777767ccc00000000
cccccccccccc33887557ccccccc555577777cccccc222222222222cccccc66d77777776cccccc777777777cccccc777776cccccccccc67777c77cccc00000000
ccccccccccc33b370790ccccccc5555555552222c555222222222cccccc5556d777777dcccccc7777777cccccccc6c676dcccccccccc77676dcccccc00000000
cccccccccccc33337777ccccccc55555551222215555557772227cccca5555667777779ccccc777777777cccccccc7777cddccccccccc7777ccccccc00000000
c33333ccccccc358888855cccc555577777111115555557777777cccc554556d7777779ccccc777777777cccccccccc6ccccccccccccccc6cccccccc00000000
c33b33cccccc88588888cccccc5557777771111ccc555577777777ccaa5555d77777779cccc77777777777cccccccc777c7cccccccccc777cccccccc00000000
c33333cccccc88577777cccccc55577777777ccccc555577777777cccaa55557777777ccccc77777777777ccccccc7c7c77ccccccccc7c7c7ccccccc00000000
ccccccccccccc5997779ccccccc5999777799cccccc55999777799ccca9c559997777ccccccc7999777799cccccc77cccccccccccccc77cc77cccccc00000000
ccccccccccccccccccccccccccccc16666ccccccccccccccccccccccccccccc55555cccccccccccccccccccccccccccc7ccccccccccccccc7ccccccc00000000
cccccccccccccccccccccccccc9666666666cccccccccc55555cccccccccc555555555ccccccc999999ccccccccccc67777ccccccccccc67777ccccc00000000
ccccccccccccccccccccccccc166666666666ccccccc555555555cccccccc5555555555cccc9999090099cccccccc7677777ccccccccc7677777cccc00000000
ccccccccccccccccccccccccc111166675557ccccccc5555555555cccccc55555775557cccc7999666666ccccccc77667777cccccccc77667777cccc00000000
cccccccccccccccccccccccc1111117777777cccccc55555775557cccccc55557777777cccc99966666666cccccc767777777ccccccc767777777ccc00000000
cc3ccc3ccccccccccccccccc1111157007990cccccc55557777777cccccc55557707990cccc96667777777cccccc777557665ccccccc777557665ccc00000000
cc33c333cccccc888ccccccc1111557777992cccccc55557707990cccccc55557707990ccccc6677007990ccccccc77557665cccccccc77557665ccc00000000
cc33bb3cccccc888888ccccccc15557777972cccccc55557707990ccccccc5557777977ccccc6777a07990ccccccc7777767ccccccccc7777767cccc00000000
cccccccccccc88888888ccccccc5557777722ccccccc225777797722ccccd6d5557777ccccccc777777977cccccccc6cc77ccccccccccc6cc77ccccc00000000
44cccccccccc88887557ccccccc5555555520000cccc222222777222cccc6555577776cccccccc7777777cccccc6777776ccccccccc6777776cccccc00000000
4aacccccccccc85707903cccccc5555555120001cccc255555222222ccc6555577777dcccccc77777777ccccccc76c676dcccccccccc7c776dcccccc00000000
caaaaacaccccc55777773cccccc5555577711111ccc5555552222255ccd6555d777776cccc7777777777cccccc7cc7777cddccccccccc7777ccccccc00000000
ccaaaaaaccccc88888885ccccc5555777771111cccc55557772227cccc66dd6777777cccccc777777777cccccc7cccc6ccccccccccccccc6cccccccc00000000
cccaa9cccccc885558885ccccc55577777777ccccc555577777777ccccd66777777777cccc77777777777ccccccccc777ccccccccccccc777c7ccccc00000000
cacaa99ccccc88577777cccccc55577777777ccccc555577777777cccc555997777777cccc77777777777ccccccccc677ccccccccccc77c7c77ccccc00000000
ccaaccccccccc5997779ccccccc5999777799cccccc55999777799ccccc55997777799ccccc7999777799ccccccccc67cccccccccccc7ccccccccccc00000000
ccc44cccccccccccccccccccccccc16666ccccccccccc55555ccccccccccc55555cccccccccc999999ccccccccc44c44c4455566000000000000000000000000
ccc4cccccccccccccccccccccc9666666666ccccccc555555555ccccccc555555555cccccc9999090099cccccccc444444555566000000000000000000000000
cccaacccccccccccccccccccc166666666666cccccc5555555555cccccc5555555555ccccc7999666666cccccccff4fff9955566000000000000000000000000
cccaacccccccccccccccccccc111166675557ccccc55555775557ccccc55555775557ccccc99966666666cccccc46f4644955566000000000000000000000000
cccaaacccccccccccccccccc1111117777777ccccc55557777777ccccc55557777777ccccc96667777777cccccc9909994955566000000000000000000000000
cccaaacccccccccccccccccc1111157707990ccccc55557707990ccccc55557707990cccccc6677a07990ccc5559797999125566000000000000000000000000
acaaaacacccccc888ccccccc1111557707990ccccc55557707990ccccc55557707990cccccc6777a07990ccc5555949941125566000000000000000000000000
caaa9aaaccccc888888ccccccc15557777977cccccc2257777977cccccc5557777977ccccccc777777977ccc5549999991122666000000000000000000000000
cccccccccccc88888888ccccccc5557777777ccccc222222277722ccccc5557777777cccccccc7777777777c5599999911666666000000000000000000000000
cccccccccccc88887557ccccccc5557777777ccccc222222222222ccccc66d77777776ccc7777777777777cc5599977666666666000000000000000000000000
cccc2222ccccc8570790cccccc555577777777ccc225222222222cccccd556d777777dcccc77777777777ccc5599977666666666000000000000000000000000
22222222ccccc5577777cccccc5515777777779cc2255577722279ccccd55567777779ccccccc777777779cc55d9977666666666000000000000000000000000
22222222ccccc8888888ccccc22221177777779cc2555577777779cccc655577777779cccccc7777777779cc55ddd77666666666000000000000000000000000
2222cccccccc88888888ccccc23321177777779cc2555577777779ccccdd5557777779cccccc7777777779cc55ddd77666666666000000000000000000000000
cccccccccccc88577779ccccc23321177777777ccc555777777777cccc555557777777ccccc7777777777cccccddd77666666dcc000000000000000000000000
ccccccccccccc5577779ccccc2222117777777cccc55577777777ccccc55777777777ccccccc77777777ccccccccd77666dccccc000000000000000000000000
ccccccccccccccccccccccccccccccccccccccccccccc55555ccccccccccc55555cccccccc99ccc99ccccccc0000000000000000000000000000000000000000
cccc2222cccccccccccccccccccc966666ccccccccc555555555ccccccc555555555cccccc79999999cccccc0000000000000000000000000000000000000000
ccc22222ccccccccccccccccc11666666666ccccccc5555555555cccccc5555555555ccccc9999090099cccc0000000000000000000000000000000000000000
2222222ccccccccccccccccc1111666666666ccccc55555775557ccccc55555775557ccccc9999666666cccc0000000000000000000000000000000000000000
22222ccccccccccccccccccc1111166675557ccccc55557777777ccccc55557777777ccccc99966666666ccc0000000000000000000000000000000000000000
222ccccccccccccccccccccc1111117777777ccccc55557707990ccccc55557707990cccccc6667777777ccc0000000000000000000000000000000000000000
cccccccccccccc888cccccccc111157707990ccccc55557707990ccccc55557707990cccccc6677a07990ccc0000000000000000000000000000000000000000
ccccccccccccc888888ccccccc11557707990cccccc2257777977cccccc5557777977cccccc6777a07990ccc0000000000000000000000000000000000000000
00000000cccc88888888cccccc15557777977ccccc222222277722ccccc5557777777ccccccc777777977ccc0000000000000000000000000000000000000000
00000000cccc88887557ccccccc5557777777ccccc222222222222ccccc66d77777776ccccccc7777777cccc0000000000000000000000000000000000000000
00000000ccccc8570790cccccc55557777777cccc225222222222cccccd556d777777dcccccc777777777ccc0000000000000000000000000000000000000000
00000000ccccc5577777cccccc55157777777cccc225557772227ccccc555667777775ccccc77777777777cc0000000000000000000000000000000000000000
00000000ccccc8888888ccccc222211797777cccc255557977777ccccc555d797777755ccc77777977777ccc0000000000000000000000000000000000000000
00000000cccc88888888ccccc2332117977777ccc255577977777cccc555dd797777755ccccc77797777cccc0000000000000000000000000000000000000000
00000000cccc88597777ccccc2332117977777cccc5557797777ccccc555577977777ccccccc77797777cccc0000000000000000000000000000000000000000
00000000ccccc5597777ccccc222211777777cccccc555777777ccccccc555777777ccccccccc777777ccccc0000000000000000000000000000000000000000
bbbbbbbbbbbbbbbb44444444bbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbbbbbbbbb44444444bbbb33bbb3bbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbb3bbbbbbbbbb44444444bb3444444453bb3444334bbb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bb3b3bbbbbbbbbbb44444444bb54444444444444444445bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
b3bb3b3bbbbbbbbb44444444bb44444444444444444444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbbd11d1db44444444b344444444444444444444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbd6666d6d44444444b34445d444444444444444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbd666666d44444444b34442444444444444d543bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
d6666666666666666666666dbb34444444444444444443bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
16666666666666666666666dbb3444444444444444444bbb00000000000000000000000000000000000000000000000000000000000000000000000000000000
d6666666666666666666666dbbb4444444444444444443bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
d66666666666666666666661bb44444444444444444445bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
d66666666666666666666661b344444444445d44444444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
16666666666666666666666db344444444455544444444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
16666666666666666666666db344454444422244444444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
d6666666666666666666666dbb44444444444244444443bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb1666666d00000000bb44444444444444d54444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb166d666100b33003bb34444444444444524443bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb1ddd1111b3bbbb3bbb44444444445444444444bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb166d11d1bbbbbbbbbb44444444442444444443bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb1d66d1d1bbbbbbbbbb34444444444444444443bb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb1dd1d1d1bbbbbbbbbbb54443b3544b3443445bbb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb11d1d1d1bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb31d33113bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000000000000
3bb3bbb300000000444443bbbb344444444444444444444400000000000000000000000000000000000000000000000000000000000000000000000000000000
133d3331000000004444445bb3444444445d44444444444400000000000000000000000000000000000000000000000000000000000000000000000000000000
1ddd11110000000044444444b44444d4455544444555444400000000000000000000000000000000000000000000000000000000000000000000000000000000
166d11d1000000004444444444444444422244444422444400000000000000000000000000000000000000000000000000000000000000000000000000000000
1d66d1d100000000444d444444444444444244444444444400000000000000000000000000000000000000000000000000000000000000000000000000000000
1dd1d1d100000000445544444444444444444453b444444400000000000000000000000000000000000000000000000000000000000000000000000000000000
11d1d1d1000000004442444444445544444444bbbb34444d00000000000000000000000000000000000000000000000000000000000000000000000000000000
d1ddd11d000000004444444444444244444443bbbb34444400000000000000000000000000000000000000000000000000000000000000000000000000000000

__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000010101000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000
__map__
e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c4c4c5c0e0e0e0e0c0e0e0c3c4c4c5e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2f2c4c4c4c4c5e0e0c3f3c2c2d5c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
d4c2c2c2c2f4e4e5e0c0d3d4c2c2f2c400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2f4e5e0e0e0e0e3e4f5c2c2c200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2c2d5c0e0e0e0e0e0e0d3c2d4c200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2f4e5e0e0e0e0e0e0c0d3c2c2c200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2c2c2d5e0e0e0e0e0c3c4c4f3c2c2c200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2f4e4e5e0e0c0e0e0d3c2c2c2c2c2c200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2d5c0e0e0e0e0e0e0e3e4f5c2c2d4c200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c2f2c4c4c4c5e0e0e0e0e0e3e4f5c2c200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e4e4e4e4e4e5c0e0e0e0e0c0e0e3e4e400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
011f00201b2401324018240132401b2401324018240132401d2401324018240132401d2401324018240132401f2401324016240132401f24013240162411f2401d2401324016240132401d240132401b2401a240
011f00200c1640c1600c1600c1600c1600c1600c1600c160081640816008160081600816008160081600816003164031600316003160031600316003160031600216402160021600216002160021600216002160
011f00101b240132401b240132401b240132401b240132401d240132401d240132401d240132401d2401324000000000000000000000000000000000000000000000000000000000000000000000000000000000
011f00201824000000182400000018240000001824000000182400000018240000001824000000182400000016240000001624000000162400000016240000001624000000162400000016240000001624000000
011f00100f25013250152501d2501b2501625022250292500e25013250162501b2501a250162501a2501b25000000000000000000000000000000000000000000000000000000000000000000000000000000000
011f00101863318603186031863318633186031860300000186331863418633186331863300000000001860400000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010400001854424542000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010500003003124031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010c00000c03130031000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010500003062330613000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01060000247510c751000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010300003072300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011600003037530365303553034530335303253031500305003050030500305003050030500305003050030500305003050030500305003050030500305003050030500305003050030500305003050030500305
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 00014344
00 00014344
00 02030145
00 02030105
00 02030105
02 04050145
02 44454144
00 41424344
00 41424344
00 41424344
03 01424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344

