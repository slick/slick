<div class="nav-home">
    <a href="$page.home.href$" $if(page.home.active)$class="active" $endif$>
        <span class="home-icon">⌂</span>$page.home.title$
    </a>
    <div class="version-number">
        <ul class="menu version-menu" data-options="disableHover: true; clickOpen: true; closeOnClickInside: false;">
            <li class="is-dropdown-submenu-parent">
                <a href="#">
                    $page.properties.("project.version")$
                </a>
                <ul class="menu">
                </ul>
            </li>
        </ul>
    </div>
</div>
$groups()$
<div class="nav-toc">
    $page.navigation$
</div>
