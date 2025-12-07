export function part1(input:string) {
  const ranges=input.split(",");
  var acc=0;
  for (const range of ranges) {

  const [start,end] = range.split('-');
  const startN = parseInt(start);
  const endN = parseInt(end);

  const halfLength = Math.floor(start.length/2);

  const startHalf = start.length % 2 == 0 ? start.substring(0,halfLength) : "1" + "0".repeat(halfLength);
  
  const startHN = parseInt(startHalf);

  var t = startHN;
  var i = parseInt(startHalf+startHalf);
  console.log("startHalf", startHalf)
  while (i<startN) {
    t++;
    const tStr = t.toString();
    i = parseInt(tStr+tStr);
  }
  while (i<=endN && i>=startN) {
    acc+=i;
    t++;
    const tStr = t.toString();
    i = parseInt(tStr+tStr);
  }
}

  return acc;
}