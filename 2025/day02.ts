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

export function part2(input: string) {
  const ranges=input.split(",");
  var acc=0;
  for (const range of ranges) {
    const counted = new Set();
    const [start,end] = range.split('-');
    const startN = parseInt(start);
    const endN = parseInt(end);
    const maxChunkSize=Math.floor(Math.max(start.length,end.length)/2);
    for (var chunkSize=1;chunkSize<=maxChunkSize;chunkSize++) {
      var numChunks=Math.floor(start.length/chunkSize);
      var firstChunk = start.substring(0,chunkSize);
      var s = parseInt(firstChunk);
      if (numChunks == 1) {
        numChunks++;
        firstChunk = "1" + "0".repeat(chunkSize-1);
      }
      const t = firstChunk.repeat(numChunks);
      var i = parseInt(t);
      console.log("chunkSize", chunkSize, "numChunks", numChunks, "s", s, "t", t)
      while (i < startN) {
        s++;
        const sStr = s.toString();
        const t = sStr.repeat(numChunks);
        i = parseInt(t);
      }
      console.log("b" ,s, i, startN, endN);
      while (i >= startN && i <= endN) {
        if (!counted.has(i)) {
          acc += i;
          counted.add(i);
          console.log("accumulating", i);
        }
        s++;
        var sStr = s.toString();
        if (sStr.length > chunkSize) {
          sStr = "1" + "0".repeat(chunkSize-1);
          s = parseInt(sStr);
          numChunks++;
          console.log("  ", "new numChunks",numChunks,sStr)
        }
        const t = sStr.repeat(numChunks);
        i = parseInt(t); 
        console.log(s,i);
      }
    }

  }
  return acc;
}