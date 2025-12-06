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
    for (const l of lines) {
        const dir=l[0];
        const num=parseInt(l.substring(1));
        const op: (a:number,b:number) => number = dir=="L" ? ((a,b) => a-b) : ((a,b) => a+b);
        
        const wasZero =pos==0;
        const numRotations=Math.floor(num/100)
        
        pos=op(pos,num);
        console.log("num",l,"midpos:",pos);

        const crossesZero=(numRotations==0 && !wasZero && (pos<0 || pos>100 ))? 1:0;
        pos=(pos+1000000000) %100;
        const atZero=!wasZero && pos==0?1:0;
        counter=counter+numRotations+crossesZero+atZero;
        console.log("pos",pos,"counter",counter,"wasZero",wasZero,"numRotations",numRotations,"crossesZero",crossesZero,"atZero",atZero)
    }
    return counter;
}