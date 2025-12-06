export function part1(input:string) {
    var pos=50;
    var outputs = [];
    const lines = input.split("\n");
    for (const l of lines) {
        const dir = l[0];
        var num = parseInt(l.substring(1));
        const op: (a:number,b:number)=>number = dir=="L" ? ((a,b) => a-b): ((a,b) => a+b);
        pos=(op(pos,num)+100000000)%100;
        outputs.push(pos);
    }
    return outputs.filter(o => o===0).length;
}

export function part2(input: string) {
    var pos=50;
    var counter=0;
    const lines=input.split("\n");
    //console.log("pos", pos);
    for (const l of lines) {
        const dir=l[0];
        const num=parseInt(l.substring(1));
        const op: (a:number,b:number) => number = dir=="L" ? ((a,b) => a-b) : ((a,b) => a+b);
        const starting = dir=="L" ? (100 - pos)%100 : pos;
        
        
        const newPos = op(pos,num)
        const numRotations=Math.round((starting+num-50)/100);
        
        
        //console.log("num",l,"starting", starting,"rotation:",starting+num-50);

        pos=(newPos+1000000000) %100;
        const atZero = numRotations == 0 && num > 0 && pos == 0? 1: 0;
        
        counter=counter+numRotations + atZero;
        //console.log("pos",pos,"counter",counter,"numRotations",numRotations, "atZero", atZero)
    }
    return counter;
}