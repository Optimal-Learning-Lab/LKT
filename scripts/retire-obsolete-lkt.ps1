param([switch]$Execute)
Set-StrictMode -Version Latest
$ErrorActionPreference='Stop'
$head='C:\dev\LKT'
. 'C:/dev/PolicyFactory2/workflows/simulation/shared/lkt_release.ps1'
$release=Resolve-LktReleaseConfig -Config @{lkt_release_file='workflows/simulation/shared/lkt-release.env'} -ProjectRoot 'C:/dev/PolicyFactory2'
$activeLibrary=$release.lkt_library.Replace('/',[string][char]92)
$archiveRoot='C:\dev\.retired-code\LKT\2026-11-03'
$report='C:\dev\LKT-outputs\retirement-20261103'
$roots=@('C:\dev\LKT','C:\dev\LKT-previous','C:\dev\PolicyFactory2',
  'C:\dev\PolicyFactory2-previous','C:\Users\ppavl\Documents\Codex',
  'C:\Users\ppavl\AppData\Local\R\win-library','C:\Program Files\R')
$roots=@($roots | Where-Object {Test-Path -LiteralPath $_})
$keep=@((Join-Path $head $release.lkt_source_archive),(Join-Path 'C:/dev/PolicyFactory2' $release.lkt_source_archive))
$releaseSha=$release.lkt_source_sha256
foreach($p in $keep){if((Get-FileHash -LiteralPath $p -Algorithm SHA256).Hash.ToLowerInvariant() -ne $releaseSha){throw "Current archive differs: $p"}}
$headManifest=Get-Content "$head\inst\research-source-manifest.json" -Raw | ConvertFrom-Json
foreach($entry in $headManifest.files){
  if((Get-FileHash -LiteralPath (Join-Path $head $entry.path) -Algorithm SHA256).Hash -ne $entry.sha256){throw "Head source differs from its verified manifest: $($entry.path)"}
}
if((Get-FileHash "$head\inst\research-source-manifest.json").Hash -ne
   (Get-FileHash "$activeLibrary\LKT\research-source-manifest.json").Hash){throw 'Installed current build differs from head source manifest'}
$files=@(& rg --files --hidden --no-ignore @roots -g DESCRIPTION -g 'LKT_*.tar.gz' -g 'LKT_*.zip' -g LKTfunctions.R -g '!**/.git/**' -g '!**/node_modules/**')
if($LASTEXITCODE -ne 0){throw 'Package inventory failed'}
$candidates=@();$apis=@()
$headExports=@(Select-String '^export\(' "$head\NAMESPACE" | ForEach-Object Line)
foreach($file in $files){
  $path=[IO.Path]::GetFullPath($file)
  if($path.EndsWith('\DESCRIPTION')){
    if((Get-Content -LiteralPath $path -First 1) -ne 'Package: LKT'){continue}
    $directory=Split-Path -Parent $path
    if($directory -eq $head -or $directory -eq "$activeLibrary\LKT"){continue}
    $namespace=Join-Path $directory 'NAMESPACE'
    if(Test-Path -LiteralPath $namespace){
      $exports=@(Select-String '^export\(' $namespace | ForEach-Object Line)
      $missing=@($exports | Where-Object {$_ -notin $headExports})
      $apis += [pscustomobject]@{Path=$directory;Exports=$exports.Count;MissingInHead=$missing}
      if($missing.Count){throw "Unaccounted public APIs in $directory : $($missing -join ', ')"}
    }
    $parent=Split-Path -Parent $directory
    if((Split-Path -Leaf $parent) -like 'LKT-2.*'){$directory=$parent}
    $candidates += $directory
  } elseif($path.EndsWith('\LKTfunctions.R')) {
    if($path -eq "$head\R\LKTfunctions.R"){continue}
    # Partial frozen package sources have no DESCRIPTION; archive their whole source folder.
    $parent=Split-Path -Parent $path
    if((Split-Path -Leaf $parent) -eq 'R'){$candidates += (Split-Path -Parent $parent)}else{$candidates += $path}
  } elseif($path -notin $keep){$candidates += $path}
}
$candidates=@($candidates | Sort-Object -Unique | Sort-Object Length)
$targets=@()
foreach($path in $candidates){
  $nested=$false
  foreach($ancestor in $targets){if($path.StartsWith($ancestor+'\',[StringComparison]::OrdinalIgnoreCase)){$nested=$true;break}}
  if(-not $nested){$targets += $path}
}
New-Item -ItemType Directory -Path $report -Force | Out-Null
$plan=@();$index=0
foreach($path in $targets){
  $index++
  if($path -eq $head -or $path -eq $activeLibrary -or $path.StartsWith($activeLibrary+'\')){throw 'Attempted to retire current code'}
  $approved=@($roots | Where-Object {$path -eq $_ -or $path.StartsWith($_+'\',[StringComparison]::OrdinalIgnoreCase)})
  if(-not $approved.Count){throw "Path outside inventoried roots: $path"}
  $item=Get-Item -LiteralPath $path -Force
  $contents=if($item.PSIsContainer){@(Get-ChildItem -LiteralPath $path -File -Recurse -Force)}else{@($item)}
  $entries=@($contents | ForEach-Object {
    if($_.Attributes -band [IO.FileAttributes]::ReparsePoint){throw "Refusing linked file $($_.FullName)"}
    [pscustomobject]@{Path=if($item.PSIsContainer){$_.FullName.Substring($path.Length+1).Replace('\','/')}else{$_.Name};
      SHA256=(Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash.ToLowerInvariant();Bytes=$_.Length}
  })
  $plan += [pscustomobject]@{Original=$path;Directory=$item.PSIsContainer;
    Archive=(Join-Path $archiveRoot ('{0:D3}-{1}.retired.zip' -f $index,$item.Name));Files=$entries}
}
$plan | ConvertTo-Json -Depth 8 | Set-Content "$report\plan.json"
$apis | ConvertTo-Json -Depth 6 | Set-Content "$report\public-api-coverage.json"
Write-Output "Verified head identity and public API coverage. Retirement targets: $($plan.Count)"
$plan | Select-Object Original,Archive | Format-Table -AutoSize
if(-not $Execute){exit 0}
if(Test-Path -LiteralPath $archiveRoot){throw 'Retirement archive already exists; review recorded results instead of rerunning'}
New-Item -ItemType Directory -Path $archiveRoot -Force | Out-Null
$hidden=Get-Item -LiteralPath 'C:\dev\.retired-code' -Force
$hidden.Attributes=$hidden.Attributes -bor [IO.FileAttributes]::Hidden
Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem
$completed=@()
foreach($target in $plan){
  if($target.Directory){
    [IO.Compression.ZipFile]::CreateFromDirectory($target.Original,$target.Archive,[IO.Compression.CompressionLevel]::Optimal,$false)
  } else {
    $zip=[IO.Compression.ZipFile]::Open($target.Archive,[IO.Compression.ZipArchiveMode]::Create)
    try{[IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip,$target.Original,(Split-Path -Leaf $target.Original)) | Out-Null}finally{$zip.Dispose()}
  }
  $zip=[IO.Compression.ZipFile]::OpenRead($target.Archive)
  try {
    $actual=@($zip.Entries | Where-Object {-not $_.FullName.EndsWith('/')})
    if($actual.Count -ne $target.Files.Count){throw 'Archive file count mismatch'}
    foreach($file in $target.Files){
      $entry=$zip.GetEntry($file.Path)
      if($null -eq $entry){throw "Missing archived entry $($file.Path)"}
      $stream=$entry.Open();$hasher=[Security.Cryptography.SHA256]::Create()
      try{$hash=[Convert]::ToHexString($hasher.ComputeHash($stream)).ToLowerInvariant()}finally{$stream.Dispose();$hasher.Dispose()}
      if($hash -ne $file.SHA256){throw 'Archive content verification failed'}
      $source=if($target.Directory){Join-Path $target.Original $file.Path}else{$target.Original}
      if((Get-FileHash -LiteralPath $source -Algorithm SHA256).Hash.ToLowerInvariant() -ne $hash){throw "Source changed during retirement: $source"}
    }
  } finally {$zip.Dispose()}
  if($target.Directory){
    $remaining=@(Get-ChildItem -LiteralPath $target.Original -File -Recurse -Force)
    if($remaining.Count -ne $target.Files.Count){throw 'Source directory changed during retirement'}
  }
  # Move only verified, explicitly inventoried paths. The archive is outside all active roots.
  $staging=Join-Path $archiveRoot 'verified-move-payload'
  if(Test-Path -LiteralPath $staging){throw 'Unresolved prior move payload'}
  Move-Item -LiteralPath $target.Original -Destination $staging
  $resolved=[IO.Path]::GetFullPath($staging)
  if(-not $resolved.StartsWith($archiveRoot+'\',[StringComparison]::OrdinalIgnoreCase)){throw 'Unsafe cleanup target'}
  Remove-Item -LiteralPath $resolved -Recurse -Force
  $completed += [pscustomobject]@{Original=$target.Original;Archive=$target.Archive;
    SHA256=(Get-FileHash -LiteralPath $target.Archive -Algorithm SHA256).Hash.ToLowerInvariant();Files=$target.Files.Count}
  $completed | ConvertTo-Json -Depth 5 | Set-Content "$archiveRoot\completed.json"
  Write-Output "RETIRED $($target.Original)"
}
Copy-Item -LiteralPath "$report\plan.json" -Destination "$archiveRoot\manifest.json"
Copy-Item -LiteralPath "$archiveRoot\completed.json" -Destination "$report\completed.json"
@('Retired LKT code: historical evidence only. These ZIPs are not R libraries or active source trees.',
  'Original paths, individual file hashes, and archive checksums are recorded in manifest.json/completed.json.',
  'Current source: C:/dev/LKT. Current installed build: '+$activeLibrary,
  'Do not restore an old package to an active library to make an old workflow run. Update the workflow to current LKT.') | Set-Content "$archiveRoot\README.txt"
