Write-Progress -Activity "Extract" -Status "Progress:" -PercentComplete 0

Remove-Item .\src\* -recurse
$ArgList = @('DESIGNER', '/F .\temp\', '/DumpExternalDataProcessorOrReportToFiles .\src\QL-Parser.xml .\build\QL-Parser.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Extract" -Status "Progress:" -PercentComplete 50

Remove-Item .\gui\src\* -recurse
$ArgList = @('DESIGNER', '/F .\temp\', '/DumpExternalDataProcessorOrReportToFiles .\gui\src\gui.xml .\build\gui.epf')
Start-Process 'C:\Program Files (x86)\1cv8\common\1cestart.exe' -ArgumentList $ArgList -Wait

Write-Progress -Activity "Extract" -Status "Progress:" -PercentComplete 100