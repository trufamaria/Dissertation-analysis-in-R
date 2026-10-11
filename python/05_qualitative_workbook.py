from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
wb=Workbook(); wb.properties.creator="Marina Petric"
F="Arial"; hf=Font(name=F,bold=True,color="FFFFFF",size=10); hfill=PatternFill("solid",fgColor="404040")
bf=Font(name=F,size=10); bold=Font(name=F,size=10,bold=True); thin=Side(style="thin",color="BFBFBF")
def sheet(ws,headers,rows,widths):
    ws.append(headers)
    for r in rows: ws.append(r)
    for c in ws[1]: c.font=hf; c.fill=hfill; c.alignment=Alignment(wrap_text=True,vertical="center")
    for row in ws.iter_rows(min_row=2):
        for c in row:
            c.font=bf; c.alignment=Alignment(wrap_text=True,vertical="top"); c.border=Border(bottom=thin)
    for i,w in enumerate(widths): ws.column_dimensions[chr(65+i)].width=w
    ws.freeze_panes="A2"
# Overview
ws=wb.active; ws.title="Group sizes"
rows=[["Right / extreme right (4-5)","US",171,114],["Right / extreme right (4-5)","Brazil",130,97],
["Left / extreme left (1-2)","US",100,68],["Left / extreme left (1-2)","Brazil",114,88],
["Center (3)","US",245,154],["Center (3)","Brazil",154,108]]
sheet(ws,["Group (Pol_Tend)","Country","Non-empty comments","Non-trivial comments","% non-trivial"],
      [r+[f"=D{i+2}/C{i+2}"] for i,r in enumerate(rows)],[30,12,20,20,14])
for i in range(2,8): ws[f"E{i}"].number_format="0%"
ws.append(["Total","",'=SUM(C2:C7)','=SUM(D2:D7)','=D8/C8'])
for c in ws[8]: c.font=bold
ws["E8"].number_format="0%"
ws["A10"]="Note. Non-trivial = comments longer than 5 characters that are not a bare 'no', 'none', 'n/a' or 'nothing'. Counts are from the 1,345-respondent merged file (Merged_BR_US_Updated.sav). Pol_Tend: 1 = extreme left ... 5 = extreme right."
ws["A10"].font=Font(name=F,size=9,italic=True); ws.merge_cells("A10:E10"); ws["A10"].alignment=Alignment(wrap_text=True,vertical="top"); ws.row_dimensions[10].height=42
# Patterns
P=[
["Right","Perceived bias against them","Say the survey leans left or is built to make the right look radical. Strongest in Brazil.","Both"],
["Right","Items seen as extreme or absurd","'Beiram o absurdo', 'bizarros', 'way too extreme'; some ask if it was satire.","Both"],
["Right","Political declarations","Use the box to state a position: Lula, Bolsonaro 2026, anti-communism, God and family, the Supreme Court (Brazil); Trump support, term limits (US).","Mostly Brazil"],
["Right","Positive evaluation","'Muito boa', 'interessante', 'excelente'; requests for more surveys and higher pay. Most favorable US group.","Both"],
["Left","Content disturbing","'Horrifying', 'triggering', 'scary', 'insulting'.","Mostly US"],
["Left","Items read as a portrait of the right","Extreme items seen as reflecting right-wing rhetoric, fascism, far-right fake news.","Both"],
["Left","Name political targets","Trump (US); extreme right, external forces, polarization (Brazil).","Both"],
["Left","Minority claim opposite bias","A few say the survey has a far-right slant or ask who commissioned it.","Mostly Brazil"],
["Left","Positive evaluation","'Interessante', 'necessária', 'me fez refletir'.","Mostly Brazil"],
["Center","Mostly about the survey itself","Neutral or positive ('interesting', 'different', 'thought provoking'), plain thanks. Least political group.","Both"],
["Center","Lack of nuance","'Nothing is as cut and dried', 'shades of gray', 'ser extremista é perigoso'.","Both"],
["Center","Explain the middle position","'Nem direita nem esquerda', 'não gosto de política', 'I'm neither left or right'.","Mostly Brazil"],
["Center","Bias claims go both ways","One US centrist says it favors the right, another that it attacks Trump; a Brazilian says it leans left.","Both"],
["Center","Anxiety about topics","'Scary', 'uncomfortable', 'afraid of the world'; some pray for peace.","Mostly US"],
]
sheet(wb.create_sheet("Patterns by group"),["Group","Theme","What respondents say","Where it appears"],P,[12,34,80,16])
# Cross
X=[["Perceived bias","Each side saw the survey as biased against it; clearest for the Brazilian right and the US left."],
["Reading of the extreme items","Right: unfair. Left: accurate portrait of the right. Center: upsetting."],
["Country","Brazilian right and left comments are more political than US ones; Brazilian centrists tend to see politics as something to avoid."]]
sheet(wb.create_sheet("Across groups"),["Topic","Pattern"],X,[30,100])
# Quotes
Q=[
["Right","BR","Pesquisa tendenciosa para a esquerda","The survey was very biased toward the left. In my opinion it is a bad survey.","Perceived bias"],
["Right","BR","A pesquisa tinha muitas pegadinhas e tem um viés ideológico voltado para a esquerda, totalmente parcial.","The survey had many tricks and a left-leaning ideological bias, totally partial.","Perceived bias"],
["Right","US","I'm unsure what this survey is about if not trying to make people on the right sound radical and unreasonable","","Perceived bias"],
["Right","BR","Maluca!!! Perguntas que beiram o absurdo","Crazy!!! Questions that border on the absurd.","Extreme/absurd items"],
["Right","US","Some of the statements and questions were way too extreme. Was the hyperbole a way to get attention to some issues that do have some merit? Was it satire?","","Extreme/absurd items"],
["Right","BR","Lula é ladrão","Lula is a thief.","Political declaration"],
["Right","BR","Eu quero que o Jair messias ganhe na nossa presidência do Brasil em 2026.","I want Jair Messias to win the presidency in 2026.","Political declaration"],
["Right","US","Yes, support President Trump 100%!!! I know I do!!!","","Political declaration"],
["Right","BR","Gostaria de participar de mais pesquisas como essa onde posso expor minhas ideias com relação a minha opção política","I would like to take more surveys like this where I can express my ideas about my political choice.","Positive"],
["Right","US","Thus is the best survey I've done yet I loved it","","Positive"],
["Left","US","The questions were horrifying.   I don't know what you're trying to find out.","","Disturbing"],
["Left","US","it was triggering","","Disturbing"],
["Left","US","The examples used were insane but reflective of the Right's rhetoric and perspective.","","Portrait of the right"],
["Left","US","I'd hate to meet the people who agreed with all the racist, sexist, violent, and science ignorant questions in this survey.","","Portrait of the right"],
["Left","BR","excelente pesquisa que aborda de uma maneira geral todos os conceitos fascistas do momento.","Excellent survey that broadly covers all the fascist concepts of the moment.","Portrait of the right"],
["Left","BR","a pesquisa tem um viés de extrema direita muito forte.","The survey has a very strong far-right bias.","Opposite bias claim"],
["Left","BR","Gostaria de saber quem encomendou essa pesquisa. Há termos muito \"fortes\" nos enunciados de várias questões.","I would like to know who commissioned this survey. There are very 'strong' terms in the wording of several questions.","Opposite bias claim"],
["Left","BR","achei interessante e me fez refletir muito","I found it interesting and it made me think a lot.","Positive"],
["Center","US","I don’t think some of the questions gave options to answer completely. Nothing is as cut and dried as some of the topics were presented.","","Lack of nuance"],
["Center","BR","Acredito que ser extremista é perigoso, devemos ter um olhar mais amplo para os dois lados.","I believe being an extremist is dangerous; we should take a broader view of both sides.","Lack of nuance"],
["Center","BR","não me posiciono de nenhum lado pois ambas tem erros, gostaria de achar o meio termo entre elas","I don't take either side because both have flaws; I would like to find the middle ground.","Explains the middle"],
["Center","US","I'm neither left or right so idk.","","Explains the middle"],
["Center","BR","Não gosto de política","I don't like politics.","Explains the middle"],
["Center","US","This has a right leaning bias to the way the questions are phrased","","Bias claim"],
["Center","US","I think this survey is just trying to Negate trump's lawlessness and corruption And how he Is ruining this country","","Bias claim"],
["Center","BR","Eu achei a pesquisa um pouco tendenciosa a promover a esquerda, porém pra mim tanto faz, estou no meio nesse caos todo em que vivemos","I found the survey a bit biased toward promoting the left, but it doesn't matter to me; I'm in the middle of all this chaos.","Bias claim"],
["Center","US","I'm afraid of the world and praying for everyone.  These were some serious questions.  I pray for Peace","","Anxiety"],
["Center","US","It was one of the most interesting surveys I have ever taken.  Great topics and questions.","","Positive about survey"],
]
ws=wb.create_sheet("Example quotes")
sheet(ws,["Group","Country","Original comment","English translation (mine, for Portuguese)","Theme"],Q,[10,10,70,60,24])
ws.auto_filter.ref=f"A1:E{len(Q)+1}"
wb.save("/mnt/user-data/outputs/Qualitative_Comments_by_Ideology.xlsx")
