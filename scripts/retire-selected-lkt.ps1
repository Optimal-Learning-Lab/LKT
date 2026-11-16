param(
  [Parameter(Mandatory)][string]$TargetsFile,
  [Parameter(Mandatory)][string]$ArchiveRoot,
  [switch]$Execute,
  [int[]]$VerifiedOtherJobIds=@()
)
# Retire an explicit, reviewed inventory after release verification. Never discover
# or delete package trees implicitly; fits/results are outside this inventory.
Set-StrictMode -Version Latest
$ErrorActionPreference='Stop'
. 'C:/dev/PolicyFactory2/workflows/simulation/shared/lkt_release.ps1'
$release=Resolve-LktReleaseConfig -Config @{lkt_release_file='workflows/simulation/shared/lkt-release.env'} -ProjectRoot 'C:/dev/PolicyFactory2'
$active=[IO.Path]::GetFullPath($release.lkt_library)
$archive=[IO.Path]::GetFullPath($ArchiveRoot)
if(-not $archive.StartsWith('C:\dev\.retired-code\LKT\',[StringComparison]::OrdinalIgnoreCase)){throw 'Archive must be under the documented LKT historical-storage root'}
if(Test-Path -LiteralPath $archive){throw 'Use a fresh retirement archive directory'}
$paths=@(Get-Content -LiteralPath $TargetsFile -Raw | ConvertFrom-Json)
if(-not $paths.Count){throw 'Empty retirement inventory'}
$allowed=@('C:\dev\PolicyFactory2\.r-libs\','C:\dev\LKT-outputs\')
$targets=@()
foreach($entry in $paths) {
  $path=[IO.Path]::GetFullPath([string]$entry)
  if(-not [IO.Path]::IsPathFullyQualified([string]$entry)){throw 'Inventory paths must be absolute'}
  if(-not @($allowed | Where-Object {$path.StartsWith($_,[StringComparison]::OrdinalIgnoreCase)}).Count){throw "Target is outside allowed artifact roots: $path"}
  if($path -eq $active -or $path.StartsWith($active+'\',[StringComparison]::OrdinalIgnoreCase) -or $active.StartsWith($path+'\',[StringComparison]::OrdinalIgnoreCase)){throw 'Cannot retire the active installation or its ancestors'}
  $item=Get-Item -LiteralPath $path -Force
  if(-not $item.PSIsContainer){throw 'Inventory must name package/source artifact directories'}
  foreach($other in $targets){if($path -eq $other -or $path.StartsWith($other+'\',[StringComparison]::OrdinalIgnoreCase) -or $other.StartsWith($path+'\',[StringComparison]::OrdinalIgnoreCase)){throw 'Overlapping retirement targets'}}
  # Explicit package roots, library containers, generated build inputs, or check
  # directories only. Reject broad experiment directories containing fitted data.
  $package=if(Test-Path -LiteralPath (Join-Path $path 'DESCRIPTION')){$path}else{Join-Path $path 'LKT'}
  if(-not (Test-Path -LiteralPath (Join-Path $package 'DESCRIPTION'))){throw "No LKT package at explicit artifact root: $path"}
  if((Get-Content -LiteralPath (Join-Path $package 'DESCRIPTION') -First 1) -ne 'Package: LKT'){throw 'Inventory contains another package'}
  $children=@(Get-ChildItem -LiteralPath $path -Recurse -Force)
  if(($item.Attributes -band [IO.FileAttributes]::ReparsePoint) -or @($children | Where-Object {$_.Attributes -band [IO.FileAttributes]::ReparsePoint}).Count){throw 'Linked artifact paths are not permitted'}
  $targets += $path
}
function Assert-NoAffectedJobs {
  $jobs=@(Get-CimInstance Win32_Process)
  foreach($job in $jobs) {
    if($job.ProcessId -eq $PID){continue}
    $command=[string]$job.CommandLine
    foreach($path in $targets) {
      if($command.Replace('/','\').IndexOf($path,[StringComparison]::OrdinalIgnoreCase) -ge 0){throw "Process $($job.ProcessId) references retirement target $path"}
    }
    if($job.Name -match '^R(script|term|gui)?[.]exe$' -and $job.ProcessId -notin $VerifiedOtherJobIds){throw "Review active R process $($job.ProcessId) before retirement"}
  }
  $jobs | Where-Object {$_.Name -match '^R(script|term|gui)?[.]exe$'} |
    Select-Object ProcessId,ParentProcessId,Name,CommandLine
}
$jobs=Assert-NoAffectedJobs
$plan=@()
foreach($path in $targets) {
  $files=@(Get-ChildItem -LiteralPath $path -Recurse -File -Force | ForEach-Object {
    [pscustomobject]@{Path=$_.FullName.Substring($path.Length+1).Replace('\','/');SHA256=(Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash.ToLowerInvariant();Bytes=$_.Length}
  })
  $plan += [pscustomobject]@{Original=$path;Archive=Join-Path $archive ('{0:D3}-{1}.retired.zip' -f ($plan.Count+1),(Split-Path $path -Leaf));Files=$files}
}
$plan | Select-Object Original,Archive | Format-Table -AutoSize
if(-not $Execute){return}
New-Item -ItemType Directory -Path $archive | Out-Null
$plan | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $archive 'manifest.json')
$jobs | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $archive 'jobs-before.json')
Add-Type -AssemblyName System.IO.Compression.FileSystem
$completed=@()
foreach($target in $plan) {
  [IO.Compression.ZipFile]::CreateFromDirectory($target.Original,$target.Archive,[IO.Compression.CompressionLevel]::Optimal,$false)
  $zip=[IO.Compression.ZipFile]::OpenRead($target.Archive)
  try {
    if(@($zip.Entries | Where-Object {-not $_.FullName.EndsWith('/')}).Count -ne $target.Files.Count){throw 'Archive file count mismatch'}
    foreach($file in $target.Files) {
      $entry=$zip.GetEntry($file.Path);if($null -eq $entry){throw 'Missing archived file'}
      $stream=$entry.Open();$hasher=[Security.Cryptography.SHA256]::Create()
      try{$sha=[Convert]::ToHexString($hasher.ComputeHash($stream)).ToLowerInvariant()}finally{$stream.Dispose();$hasher.Dispose()}
      if($sha -ne $file.SHA256 -or (Get-FileHash -LiteralPath (Join-Path $target.Original $file.Path) -Algorithm SHA256).Hash.ToLowerInvariant() -ne $sha){throw 'Archive/source hash verification failed'}
    }
  } finally {$zip.Dispose()}
  if(@(Get-ChildItem -LiteralPath $target.Original -Recurse -File -Force).Count -ne $target.Files.Count){throw 'Artifact changed during retirement'}
  Assert-NoAffectedJobs | Out-Null
  # Path was resolved and checked against explicit roots and the active release
  # before archival. Native PowerShell performs the verified removal directly.
  Remove-Item -LiteralPath $target.Original -Recurse -Force
  $completed += [pscustomobject]@{Original=$target.Original;Archive=$target.Archive;SHA256=(Get-FileHash -LiteralPath $target.Archive -Algorithm SHA256).Hash.ToLowerInvariant();Files=$target.Files.Count}
  $completed | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $archive 'completed.json')
  Write-Output "RETIRED $($target.Original)"
}
