-- for some stuff, we want vector math - so we make a vector type with all the usual operations
vec={}

function vec.__add(v1,v2)
	return v(v1.x+v2.x,v1.y+v2.y)
end

function vec.__sub(v1,v2)
	return v(v1.x-v2.x,v1.y-v2.y)
end

function vec.__mul(v1,a)
	return v(v1.x*a,v1.y*a)
end

function vec.__mul(v1,a)
	return v(v1.x*a,v1.y*a)
end

function vec.__div(v1,a)
	return v(v1.x/a,v1.y/a)
end

-- we use the ^ operator to mean dot product
function vec.__pow(v1,v2)
	return v1.x*v2.x+v1.y*v2.y
end

function vec.__unm(v1)
	return v(-v1.x,-v1.y)
end

-- this is not really the length of the vector, but length squared - easier to calculate, and can be used for most of the same stuff
function vec.__len(v1)
	local x,y=v1:split()
	return x*x+y*y
end

-- normalized vector
function vec:norm()
	return self/sqrt(#self)
end

-- rotated 90-deg clockwise
function vec:rotcw()
	return v(-self.y,self.x)
end

-- force coordinates to integers
function vec:ints()
	return v(flr(self.x),flr(self.y))
end

-- used for destructuring, i.e.:  x,y=v:split()
function vec:split()
	return self.x,self.y
end

-- has to be there so our metatable works for both operators and methods
vec.__index = vec

-- creates a new vector
function v(x,y)
	local vector={x=x,y=y}
	setmetatable(vector,vec)
	return vector
end

-- vector for each cardinal direction, ordered the same way pico-8 d-pad is
dirs={
	v(-1,0),v(1,0),
	v(0,-1),v(0,1)
}
